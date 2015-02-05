package ast

case class Variable(id: Id) extends Stat {
    override def toString = id.toString
}

case class New(id: Id, stats: Stats) extends Stat {
    override def toString = "new %s = %s".format(id, stats)
}

case class Assign(id: Id, stats: Stats) extends Stat {
    override def toString = "%s = %s".format(id, stats)
}
