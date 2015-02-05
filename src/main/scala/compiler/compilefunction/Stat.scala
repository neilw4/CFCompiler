package compiler.compilefunction

import compiler._

import scala.math._

case class Stat(node: ast.Stat) extends ReturnCompiler with VoidCompiler {
    // Find the type of the stat and compile it.
    // Need to use case statements because method overloading cannot differentiate between runtime types.
    override def ret(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) =
        (node match {
            case n: ast.IntConstant   => IntConstant(n)
            case n: ast.BoolConstant  => BoolConstant(n)
            case n: ast.FloatConstant => FloatConstant(n)
            case n: ast.New           => New(n)
            case n: ast.Variable      => Variable(n)
            case n: ast.Assign        => Assign(n)
            case n: ast.Line          => Line(n)
            case n: ast.Group         => Group(n)
            case n: ast.Add           => Add(n)
            case n: ast.Subtract      => Subtract(n)
            case n: ast.Multiply      => Multiply(n)
            case n: ast.Divide        => Divide(n)
            case n: ast.LessThan      => LessThan(n)
            case n: ast.LessOrEqual   => LessOrEqual(n)
            case n: ast.Equal         => Equal(n)
            case n: ast.Not           => Not(n)
            case _                    => throw SingleCompileException(line, node, "Expected a return value")
        }).ret(vars, gotos, labelsUsed, localsInUse, line)

    override def void(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) =
        (node match {
            case n: ast.Print    => Print(n)
            case n: ast.New      => New(n)
            case n: ast.Assign   => Assign(n)
            case n: ast.Line     => Line(n)
            case n: ast.Group    => Group(n)
            case n: ast.Label    => Label(n)
            case n: ast.Comefrom => Comefrom(n)
            case n: ast.Unwhile  => Unwhile(n)
            case _               => throw SingleCompileException(line, node, "Unused statement")
        }).void(vars, gotos, labelsUsed, localsInUse, line)
}

case class Stats(stats: ast.Stats) extends ReturnCompiler with VoidCompiler {
    // If there is only one thing in the stats we can just compile it.  Anything more complicated requires a StatGroup.
    val compiler: ReturnCompiler with VoidCompiler
        = if (stats.stats.size == 1)
            Stat(stats.stats(0))
          else
            StatGroup(stats)

    override def ret(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line])
        = compiler.ret(vars, gotos, labelsUsed, localsInUse, line)

    override def void(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line])
        = compiler.void(vars, gotos, labelsUsed, localsInUse, line)

    // Compiles a group of multiple sequential stats.
    case class StatGroup(statGroup: ast.Stats) extends ReturnCompiler with VoidCompiler {
        val stats = statGroup.stats

        override def ret(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line])
            = if (stats.isEmpty)
                throw SingleCompileException(line, statGroup, "Expected a return value but got empty stats")
              else
                throw SingleCompileException(line, statGroup, "Multiple stats cannot have a return type")

        override def void(tVars: Map[ast.Id, (CFType, Int)], tgotos: Map[ast.Id, Goto], tLabelsUsed: Int, tLocalsInUse: Int, tLine: Option[ast.Line]) = {
            // All exceptions that have been caught so far.
            var exceptions = Seq[CompileException]()
            var code = List[String]()
            var vars = tVars
            var labelsUsed = tLabelsUsed
            var localsInUse = tLocalsInUse
            var localsUsed = 0
            var stackUsed = 0
            var gotos = Map[ast.Id, Goto]()

            for (stat <- stats) {
                try {
                    val result = Stat(stat).void(vars, gotos, labelsUsed, localsInUse, tLine)
                    // If new variables have been created, there is a risk of a comefrom skipping the creation.
                    if (result.newVars.nonEmpty) {
                        // Comefrom skips variable creation iff the label statement has been seen but the comefrom statement hasn't.
                        val brokenLabels = gotos.values.filter(!_.hasComefrom)
                        if (brokenLabels.size > 0)
                            throw MultiCompileException(
                                brokenLabels.map(
                                    _.label match {
                                        case (label, line) =>
                                            SingleCompileException(line, label, "Comefrom cannot skip variable declarations")
                                    }
                                ).toSeq
                            )
                    }
                    // Record new labels.
                    gotos = gotos ++ result.newLabels
                    code ++= result.code
                    // If the statement used more locals than the previous statement, record it.
                    localsUsed = max(localsUsed, localsInUse + result.localsUsed)
                    vars ++= result.newVars
                    // Number of locals being used includes newly declared variables.
                    localsInUse += result.newVars.size
                    labelsUsed += result.labelsUsed
                    // if the statement used more stack frames than the previous statement, record it.
                    stackUsed = max(stackUsed, result.stackUsed)
                } catch {
                    // Record exceptions thrown to be used later.
                    case e: SingleCompileException => exceptions ++= Seq(e)
                    case MultiCompileException(es) => exceptions ++= es
                }
            }

            // If a label has not encountered its relevant comefrom statement here, that's an error.
            val labelsMissingComefrom = gotos.values.filter(!_.hasComefrom)
            exceptions ++= labelsMissingComefrom.map(
                _.label match {
                case (label, line) => SingleCompileException(line, label, "No matching comefrom")
            })

            // The same if a comefrom has not encountered its relevant label.
            val comefromMissingLabel = gotos.values.filter(!_.hasLabel)
            exceptions ++= comefromMissingLabel.map(
                _.comefrom match {
                case (comefrom, line) => SingleCompileException(line, comefrom, "No matching label")
            })

            //If any exceptions have been caught, throw them all.
            if (exceptions.nonEmpty) {
                if (exceptions.length == 1)
                    throw exceptions(0)
                else
                    throw MultiCompileException(exceptions)
            }
            CompileResult(code, labelsUsed, localsUsed, stackUsed)
        }
    }
}
