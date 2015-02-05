package compiler

import ast.{Label, Line, Comefrom}

object Goto {
    def name(index: Int) = "label" + index
}

// Abstraction representing a comefrom statement and its corresponding label.
trait Goto {
    def index: Int
    def hasComefrom: Boolean
    def hasLabel: Boolean
    def comefrom: (ast.Comefrom, Option[ast.Line])
    def label: (ast.Label, Option[ast.Line])
    def withComefrom(comefrom: ast.Comefrom, line: Option[ast.Line]): Goto
    def withLabel(label: ast.Label, line: Option[ast.Line]): Goto

    def name = Goto.name(index)
}

case class CompleteGoto(index: Int, comefrom: (ast.Comefrom, Option[ast.Line]), label: (ast.Label, Option[ast.Line])) extends Goto {
    val hasComefrom = true
    val hasLabel = true

    override def withComefrom(newComefrom: Comefrom, line: Option[Line])
        = throw SingleCompileException(line, newComefrom, "already seen")

    override def withLabel(newLabel: Label, line: Option[Line])
        = throw SingleCompileException(line, newLabel, "already seen")
}

case class LabelGoto(index: Int, label: (ast.Label, Option[ast.Line])) extends Goto {
    val hasComefrom = false
    val hasLabel = true

    override def comefrom = throw new Exception("Shouldn't be here")

    override def withComefrom(comefrom: Comefrom, line: Option[Line]) = CompleteGoto(index, (comefrom, line), label)

    override def withLabel(newLabel: Label, line: Option[Line])
    = throw SingleCompileException(line, newLabel, "already seen")
}

case class ComefromGoto(index: Int, comefrom: (ast.Comefrom, Option[ast.Line])) extends Goto {
    val hasComefrom = true
    val hasLabel = false

    override def label = throw new Exception("Shouldn't be here")

    override def withComefrom(newComefrom: Comefrom, line: Option[Line])
        = throw SingleCompileException(line, newComefrom, "already seen")

    override def withLabel(newLabel: Label, line: Option[Line])
        = throw SingleCompileException(line, newLabel, "already seen")
}
