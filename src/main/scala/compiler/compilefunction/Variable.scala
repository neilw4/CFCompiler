package compiler.compilefunction

import compiler._

import scala.math._

case class New(newVariable: ast.New) extends ReturnCompiler with VoidCompiler {
    /*
     * load onto stack
     * dup
     * istore 0
     */
    override def ret(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) = {
        if (vars.contains(newVariable.id)) {
            throw SingleCompileException(line, newVariable, "cannot declare variable twice")
        }
        val (result, varType) = Stats(newVariable.stats).ret(vars, gotos, labelsUsed, localsInUse, line)
        val varIndex = localsInUse + result.localsUsed
        val cmdPrefix = varType.prefix
        val code = result.code ++ Seq("dup", cmdPrefix + "store " + varIndex)
        (CompileResult(
                code,
                Map(newVariable.id -> (varType, varIndex)),
                labelsUsed,
                result.localsUsed + 1,
                max(2, result.stackUsed + 1)
            ), varType)
    }

    /*
     * load onto stack
     * istore 0
     */
    override def void(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) = {
        if (vars.contains(newVariable.id)) {
            throw SingleCompileException(line, newVariable, "cannot declare variable twice")
        }
        val (result, varType) = Stats(newVariable.stats).ret(vars, gotos, labelsUsed, localsInUse, line)
        val varIndex = localsInUse + result.localsUsed
        val cmdPrefix = varType.prefix

        val code = result.code ++ Seq(cmdPrefix + "store " + varIndex)
        CompileResult(
            code,
            Map(newVariable.id -> (varType, varIndex)),
            labelsUsed,
            result.localsUsed + 1,
            result.stackUsed + 1
        )
    }
}

case class Variable(variable: ast.Variable) extends ReturnCompiler {
    /*
     * iload 0
     */
    override def ret(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) = {
        val (varType, localIndex) = vars.get(variable.id) match {
            case None        => throw SingleCompileException(line, variable, variable.id.name + " not found")
            case Some(index) => index
        }
        (CompileResult(Seq(varType.prefix + "load " + localIndex), labelsUsed, 0, 1), varType)
    }
}

case class Assign(assign: ast.Assign) extends ReturnCompiler with VoidCompiler {
    /*
     * load onto stack
     * dup
     * istore 0
     */
    override def ret(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) = {
        // Find the variable index (e.g. istore 0 has index 0).
        val (varType, index) = vars.get(assign.id) match {
            case None    => throw SingleCompileException(line, assign, assign.id.name + " not found")
            case Some(x) => x
        }
        val (result, retType) = Stats(assign.stats).ret(vars, gotos, labelsUsed, localsInUse, line)
        // Check the expression type matches the variable type.
        if (varType != retType) throw SingleCompileException(line, assign, "expected " + varType + " but got " + retType)
        val code =  result.code ++
                    Seq(
                        "dup",
                        varType.prefix + "store " + index
                    )
        (CompileResult(code, labelsUsed, result.localsUsed, max(2, result.stackUsed)), retType)
    }
    /*
     * load onto stack
     * istore 0
     */
    override def void(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) = {
        // Find the variable index (e.g. istore 0 has index 0).
        val (varType, index) = vars.get(assign.id) match {
            case None    => throw SingleCompileException(line, assign, assign.id.name + " not found")
            case Some(x) => x
        }
        val (result, retType) = Stats(assign.stats).ret(vars, gotos, labelsUsed, localsInUse, line)
        // Check the expression type matches the variable type.
        if (varType != retType) throw SingleCompileException(line, assign, "expected " + varType + " but got " + retType)
        val cmdPrefix = varType.prefix
        val code = result.code ++ Seq(cmdPrefix + "store " + index)
        CompileResult(code, labelsUsed, result.localsUsed, result.stackUsed)
    }
}
