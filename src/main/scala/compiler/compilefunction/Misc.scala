package compiler.compilefunction

import compiler._

import math.max

import ast.Id

case class Class(node: ast.Class) extends VoidCompiler {
    /*
     * class public MyClass
     * rubbish
     * method public static main([Ljava/lang/String;)V",
     * .limit stack 14
     * .limit locals 5
     * body
     * return
     * .end method
     */
    override def void(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) = {
        // Compile the rest of the program.
        val result = Stats(node.stats).void(vars, gotos, labelsUsed, localsInUse, line)
        val localsUsed = max(1, result.localsUsed) // main function argument is stored.
        val code =
            Seq(
                ".class public " + node.id.name,
                ".super java/lang/Object",
                ".method public <init>()V",
                "aload_0",
                "invokenonvirtual java/lang/Object/<init>()V",
                "return",
                ".end method",
                ".method public static main([Ljava/lang/String;)V",
                ".limit stack " + result.stackUsed,
                ".limit locals " + localsUsed
            ) ++
            result.code ++
            Seq(
                "return",
                ".end method"
            )
        CompileResult(code, labelsUsed, localsUsed, result.stackUsed)
    }
}

case class Line(line: ast.Line) extends ReturnCompiler with VoidCompiler {
    // Compile classes for all descendants now know what line the child is in.
    override def ret(vars: Map[Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, oldLine: Option[ast.Line]) =
        Stats(line.stats).ret(vars, gotos, labelsUsed, localsInUse, Some(line))

    override def void(vars: Map[Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, oldLine: Option[ast.Line]) =
        Stats(line.stats).void(vars, gotos, labelsUsed, localsInUse, Some(line))
}

case class Group(stat: ast.Group) extends ReturnCompiler with VoidCompiler {
    // Compile child as normal.
    override def ret(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) =
        Stats(stat.stats).ret(vars, gotos, labelsUsed, localsInUse, line)

    override def void(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) =
        Stats(stat.stats).void(vars, gotos, labelsUsed, localsInUse, line)
}

case class Print(print: ast.Print) extends VoidCompiler {
    /*
     * getstatic java/lang/System/out Ljava/io/PrintStream;
     * load argument
     * invokevirtual java/io/PrintStream/println(Z)V
     */
    override def void(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) = {
        val (result, returnType) = Stats(print.stats).ret(vars, gotos, labelsUsed, localsInUse, line)
        val argType = returnType.signature
        val code: Seq[String] =
            Seq("getstatic java/lang/System/out Ljava/io/PrintStream;") ++
            result.code ++
            Seq("invokevirtual java/io/PrintStream/println(" + argType + ")V")
        CompileResult(code, labelsUsed, result.localsUsed, result.stackUsed + 1)
    }
}
