package ast

case class Label(id: Id) extends Stat {
    override def toString = "label %s".format(id)
}

case class Comefrom(id: Id) extends Stat {
    override def toString = "comefrom %s".format(id)
}

object Unwhile {
    def apply(condition: Stats, stats: Stat*) = new Unwhile(condition, stats: _*)
}
case class Unwhile(condition: Stats, body: Stats) extends Stat {
    def this(condition: Stats, body: Stat*) = this(condition, Stats(body: _*))
    override def toString = "while %s dont %s".format(condition, body)
}
