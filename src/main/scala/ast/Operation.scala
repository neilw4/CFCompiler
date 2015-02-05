package ast

trait BinaryOperation extends Stat {
    def pre: Stats
    def post: Stats
    def symbol: String
    override def toString = "%s %s %s".format(pre, symbol, post)
}

case class Add(pre: Stats, post: Stats) extends BinaryOperation {
    val symbol = "+"
}

case class Subtract(pre: Stats, post: Stats) extends BinaryOperation {
    val symbol = "-"
}

case class Multiply(pre: Stats, post: Stats) extends BinaryOperation {
    val symbol = "*"
}

case class Divide(pre: Stats, post: Stats) extends BinaryOperation {
    val symbol = "/"
}

case class LessThan(pre: Stats, post: Stats) extends BinaryOperation {
    val symbol = "<"
}

case class LessOrEqual(pre: Stats, post: Stats) extends BinaryOperation {
    val symbol = "<="
}

case class Equal(pre: Stats, post: Stats) extends BinaryOperation {
    val symbol = "=="
}

case class Not(stats: Stats) extends Stat {
    override def toString = "not %s".format(stats)
}