package ast

trait Constant[T] extends Stat {
    def value: T
    override def toString = value.toString
}

case class BoolConstant(value: Boolean) extends Constant[Boolean]

case class IntConstant(value: Int) extends Constant[Int]

case class FloatConstant(value: Float) extends Constant[Float]
