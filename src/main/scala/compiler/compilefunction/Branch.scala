package compiler.compilefunction

import ast.Id
import compiler._

import scala.math.max

case class Unwhile(unwhile: ast.Unwhile) extends VoidCompiler {
    /*
     * start:
     * eval condition
     * ifne end
     * eval body
     * goto start
     * end:
     */
    override def void(vars: Map[Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) = {
        val (condition, conditionType) = Stats(unwhile.condition).ret(vars, gotos, labelsUsed, localsInUse, line)
        if (conditionType != CFBool) {
            throw SingleCompileException(line, unwhile.condition, "expected a boolean")
        }
        val body = Stats(unwhile.body).void(vars, gotos, condition.labelsUsed, localsInUse, line)
        // Make a label to allow us to goto the start of the loop.
        val startLabel = Goto.name(body.labelsUsed)
        // And a label to allow us to goto the end of the loop.
        val endLabel = Goto.name(body.labelsUsed + 1)
        val code = Seq(startLabel + ":") ++
                    condition.code ++
                    Seq("ifne " + endLabel) ++
                    body.code ++
                    Seq(
                        "goto " + startLabel,
                        endLabel + ":"
                    )
        CompileResult(code, body.labelsUsed + 2, max(condition.localsUsed, body.localsUsed), max(condition.stackUsed, 1 + body.localsUsed))
    }
}

case class Comefrom(comefrom: ast.Comefrom) extends VoidCompiler {
    /*
     * labelN:
     */
    override def void(vars: Map[Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) = {
        val (goto, labelsUsedHere) = gotos.get(comefrom.id) match {
            case None => (ComefromGoto(labelsUsed, (comefrom, line)), 1)
            case Some(origGoto) => (origGoto.withComefrom(comefrom, line), 0)
        }
        // Create a label here.
        val code = Seq(goto.name + ":")
        CompileResult(code, (comefrom.id, goto), labelsUsedHere, 0, 0)
    }
}

case class Label(label: ast.Label) extends VoidCompiler {
    /*
     * goto labelN
     */
    override def void(vars: Map[Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) = {
        val (goto, labelsUsedHere) = gotos.get(label.id) match {
            case None => (LabelGoto(labelsUsed, (label, line)), 1)
            case Some(origGoto) => (origGoto.withLabel(label, line), 0)
        }
        // Create a goto here.
        val code = Seq("goto " + goto.name)
        CompileResult(code, (label.id, goto), labelsUsedHere, 0, 0)
    }
}
