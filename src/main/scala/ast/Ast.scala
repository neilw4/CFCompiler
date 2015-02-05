package ast

// Thrown if there are problems while generating the AST.
class TreeException(msg: String) extends Exception(msg)

// Interface for all AST nodes.
trait Node
trait Stat extends Node

// <magic>
// import ast.ImplicitConversions._ to enable implicit conversions to AST types.
// e.g. compiler.Print("x") converts to compiler.Print(compiler.Variable(Id("x"))).
object ImplicitConversions {
    implicit def stringToId(name: String) = Id(name)
    implicit def stringToVar(name: String) = Variable(name)
    implicit def stringToVarStats(name: String) = Stats(Variable(name))
    implicit def intToConstant(i: Int) = IntConstant(i)
    implicit def floatToConstant(f: Float) = if (f.isNaN) Divide(floatToStats(0f), floatToStats(0f)) else FloatConstant(f)
    implicit def boolToConstant(b: Boolean) = BoolConstant(b)
    implicit def statToStats(s: Stat) = Stats(s)
    implicit def intToStats(i: Int) = Stats(intToConstant(i))
    implicit def floatToStats(f: Float): Stats = Stats(floatToConstant(f))
    implicit def boolToStats(b: Boolean) = Stats(boolToConstant(b))
    implicit def statsToGroup(s: Stats) = Group(s)
}
// </magic>
