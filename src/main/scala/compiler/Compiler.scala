package compiler

import java.io.PrintStream

// Main compiler.
object Compiler {
    def compile(node: ast.Class, out: PrintStream): Unit = out.println(compile(node))

    def compile(node: ast.Class): String = {
        // Compile the class.
        val result = compilefunction.Class(node).void(Map(), Map(), 0, 0, None)
        result.code.mkString("\n")
    }

}

// Enum of types supported by CF.
sealed trait CFType {
    // instruction prefix (e.g. i for iadd).
    def prefix: String
    // Parameter signature (e.g. Print(Z)).
    def signature: String
}
case object CFInt extends CFType {
    val prefix = "i"
    val signature = "I"
}
case object CFFloat extends CFType {
    val prefix = "f"
    val signature = "F"
}
case object CFBool extends CFType {
    val prefix = "i"
    val signature = "Z"
}

// Compiler class where the bytecode will leave the stack in its original form.
trait VoidCompiler {
    /*
     * vars - Variables currently in the scope, with their id, type and index.
     * labels - Labels currently in the scope.
     * labelsUsed - Total number of labels that have been used (stops new label names clashing with existing ones).
     * localsInUse - Number of local variables currently in the scope (to avoid overwriting).
     * line - Parent ast.Line object for comparison. May be None.
     */
    def void(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]): CompileResult
}

// Compiler class where the bytecode will leave something on top of the stack.
// Returns the type of what is on the stack as well as the CompileResult.
trait ReturnCompiler {
    /*
     * vars - Variables currently in the scope, with their id, type and index.
     * labels - Labels currently in the scope.
     * labelsUsed - Total number of labels that have been used (stops new label names clashing with existing ones).
     * localsInUse - Number of local variables currently in the scope (to avoid overwriting).
     * line - Parent ast.Line object for comparison. May be None.
     */
    def ret(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]): (CompileResult, CFType)
}


/* Compiling functions should return a CompileResult instance.
 * @param code - Seq of lines of jasmin assembler.
 * @param newVars - any new variable that have been created.
 * @param localsUsed - number of local variables used.
 * @param stackUsed - number of stack frames used.
 */
case class CompileResult(code: Seq[String], newVars: Map[ast.Id, (CFType, Int)], newLabels: Map[ast.Id, Goto], labelsUsed: Int, localsUsed: Int, stackUsed: Int) {
    def this(code: Seq[String], labelsUsed: Int, localsUsed: Int, stackUsed: Int)
        = this(code, Map(), Map(), labelsUsed, localsUsed, stackUsed)

    def this(code: Seq[String], newVars: Map[ast.Id, (CFType, Int)], labelsUsed: Int, localsUsed: Int, stackUsed: Int)
        = this(code, newVars, Map(), labelsUsed, localsUsed, stackUsed)

    def this(code: Seq[String], newLabel: (ast.Id, Goto), labelsUsed: Int, localsUsed: Int, stackUsed: Int)
        = this(code, Map(), Map(newLabel), labelsUsed, localsUsed, stackUsed)
}

object CompileResult {
    def apply(code: Seq[String], labelsUsed: Int, localsUsed: Int, stackUsed: Int) =
        new CompileResult(code, labelsUsed, localsUsed, stackUsed)

    def apply(code: Seq[String], newVars: Map[ast.Id, (CFType, Int)], labelsUsed: Int, localsUsed: Int, stackUsed: Int) =
        new CompileResult(code, newVars, labelsUsed, localsUsed, stackUsed)

    def apply(code: Seq[String], newLabel: (ast.Id, Goto), labelsUsed: Int, localsUsed: Int, stackUsed: Int) =
        new CompileResult(code, newLabel, labelsUsed, localsUsed, stackUsed)
}



abstract class CompileException(msg: String) extends Exception(msg)

object SingleCompileException {
    def msg(line: Option[ast.Line], node: ast.Node, error: String) = line match {
        case None => error + "\nin\n\t" + node
        case Some(lineNode) => error + "\nat line " + lineNode.line + "\nin\n\t" + node + "\nin line\n\t" + lineNode
    }
}
case class SingleCompileException(line: Option[ast.Line], node: ast.Node, error: String) extends CompileException(SingleCompileException.msg(line, node, error))


object MultiCompileException {
    def msg(exceptions: Seq[CompileException]) = exceptions.map(_.getMessage).mkString("\n\n")
}
case class MultiCompileException(exceptions: Seq[CompileException]) extends CompileException(MultiCompileException.msg(exceptions))
