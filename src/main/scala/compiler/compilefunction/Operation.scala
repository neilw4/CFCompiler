package compiler.compilefunction

import ast.Id

import math.max

import compiler._

trait BinaryOperation extends ReturnCompiler {
    def op: ast.BinaryOperation

    // bytecode commands to use (e.g. * uses imul and fmul).
    def command(argTypes: CFType): Seq[String]

    // Supported argument types (e.g. * supports CFInt and CFFloat).
    def supportedArgs: Seq[CFType]

    // Expected return type of the operator (e.g. * returns a CFInt or CFFloat).
    // Defaults to the same as the argument types.
    def getRetType(argTypes: CFType): CFType = argTypes

    /*
     * load first
     * load second
     * maybe coerce int to float
     * do command
     */
    override def ret(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) = {
        val (pre, preType) = Stats(op.pre).ret(vars, gotos, labelsUsed, localsInUse, line)
        val (post, postType) = Stats(op.post).ret(vars, gotos, pre.labelsUsed, localsInUse, line)
        // Check that both return types are supported.
        if (!supportedArgs.contains(preType) || !supportedArgs.contains(postType))
            throw SingleCompileException(line, op, "cannot operate on a " + preType + " and a " + postType)
        val (codeOnPre, codeOnPost, retType) = (preType, postType) match {
            case (CFInt, CFInt)     => (Seq(), command(CFInt), getRetType(CFInt))
            case (CFFloat, CFFloat) => (Seq(), command(CFFloat), getRetType(CFFloat))
            // Coerce int to float.
            case (CFFloat, CFInt)   => (Seq(), Seq("i2f") ++ command(CFFloat), getRetType(CFFloat))
            case (CFInt, CFFloat)   => (Seq("i2f"), command(CFFloat), getRetType(CFFloat))
            case (CFBool, CFBool)   => (Seq(), command(CFBool), getRetType(CFBool))
            case (t1, t2)           => throw SingleCompileException(line, op, "cannot operate on a " + t1 + " and a " + t2)
        }
        val code = pre.code ++
                    codeOnPre ++
                    post.code ++
                    codeOnPost
        (CompileResult(code, labelsUsed, max(pre.localsUsed, post.localsUsed), max(pre.stackUsed, 1 + post.stackUsed)), retType)
    }
}

case class Add(op: ast.Add) extends BinaryOperation {
    val supportedArgs = Seq(CFInt, CFFloat)
    override def command(argType: CFType) = Seq(argType.prefix + "add")
}

case class Divide(op: ast.Divide) extends BinaryOperation {
    val supportedArgs = Seq(CFInt, CFFloat)
    override def command(argType: CFType) = Seq(argType.prefix + "div")
}
case class Equal(op: ast.Equal) extends BinaryOperation {
    val supportedArgs = Seq(CFInt, CFFloat, CFBool)

    override def command(argType: CFType) = argType match {
        case CFBool => command(CFInt)
        case CFInt => Seq(
                "if_icmpeq $+7",
                "iconst_0",
                "goto $+4",
                "iconst_1"
            )
        case CFFloat => Seq(
                "fcmpg",
                "ifeq $+7",
                "iconst_0",
                "goto $+4",
                "iconst_1"
            )
    }
    override def getRetType(argType: CFType) = CFBool
}

case class LessOrEqual(op: ast.LessOrEqual) extends BinaryOperation {
    val supportedArgs = Seq(CFInt, CFFloat)

    override def getRetType(argType: CFType) = CFBool

    override def command(argType: CFType) = argType match {
        case CFInt => Seq(
                "if_icmple $+7",
                "iconst_0",
                "goto $+4",
                "iconst_1"
            )
        case CFFloat => Seq(
                "fcmpg",
                "ifle $+7",
                "iconst_0",
                "goto $+4",
                "iconst_1"
            )
        // This should never happen.
        case _ => throw new Exception("Shouldn't be here")
    }
}
case class LessThan(op: ast.LessThan) extends BinaryOperation {
    val supportedArgs = Seq(CFInt, CFFloat)
    override def command(argType: CFType) = argType match {
        case CFInt => Seq(
            "if_icmplt $+7",
            "iconst_0",
            "goto $+4",
            "iconst_1")
        case CFFloat => Seq(
            "fcmpg",
            "iflt $+7",
            "iconst_0",
            "goto $+4",
            "iconst_1")
        case _ => throw new Exception("Shouldn't be here")
    }
    override def getRetType(argType: CFType) = CFBool
}

case class Multiply(op: ast.Multiply) extends BinaryOperation {
    val supportedArgs = Seq(CFInt, CFFloat)
    override def command(argType: CFType) = Seq(argType.prefix + "mul")
}
case class Subtract(op: ast.Subtract) extends BinaryOperation {
    val supportedArgs = Seq(CFInt, CFFloat)
    override def command(argType: CFType) = Seq(argType.prefix + "sub")
}

case class Not(not: ast.Not) extends ReturnCompiler {
    override def ret(vars: Map[Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) = {
        val (result, resultType) = Stats(not.stats).ret(vars, gotos, labelsUsed, localsInUse, line)
        if (resultType != CFBool) {
            throw SingleCompileException(line, not.stats, "only boolean values can be negated")
        }
        val code = result.code ++
                Seq(
                    "ifne $+7",
                    "iconst_1",
                    "goto $+4",
                    "iconst_0"
                )
        (CompileResult(code, result.labelsUsed, result.localsUsed, max(1, result.stackUsed)), CFBool)
    }
}
