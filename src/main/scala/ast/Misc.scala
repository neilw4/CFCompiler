package ast

object Class {
    //TODO: fix this. Fix what?
    def apply(id: Id, stats: Stat*) = new Class(id, stats: _*)
}
case class Class(id: Id, stats: Stats) extends Node {
    def this(id: Id, stats: Stat*)
        = this(
            id,
            // Automatically add lines in this case.
            Stats(stats.zipWithIndex.map(Line(_)): _*)
        )
    override def toString = "class %s\n%s".format(id, stats)
}

case class Stats(stats: Stat*) extends Node {
    override def toString =
        if (stats.length == 1) {
            stats(0).toString
        } else {
            "{\n" + stats.map(_.toString + "; ").mkString("\n") + "\n}"
        }
}

case class Print(stats: Stats) extends Stat {
    override def toString = "print %s".format(stats)
}

case class Group(stats: Stats) extends Stat {
    override def toString = "(%s)".format(stats)
}

case class Id(name: String) extends Node {
    val idMatchRegex = "[a-zA-Z_]+[a-zA-Z_0-9]*"

    if (!name.matches(idMatchRegex)) {
        throw new TreeException("Bad format id: " + name)
    }

    override def toString = name
}

object Line {
    def apply(line: Int, stats: Stat*) = new Line(line, stats: _*)
    def apply(tupled: (Stat, Int)) = new Line(tupled)
}

case class Line(line: Int, stats: Stats) extends Stat {
    def this(line: Int, stats: Stat*) = this(line, Stats(stats: _*))
    def this(tupled: (Stat, Int)) = this(tupled._2, tupled._1)
    // Make sure everything is on one line.
    override def toString = stats.toString.replace("\n", "")
}
