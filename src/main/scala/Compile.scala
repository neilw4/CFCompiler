import java.io.PrintStream
import compiler.Compiler

// Contains functions that can be called from outside.
object Compile {
    def compile(node: ast.Class): String = Compiler.compile(node)
    def compile(node: ast.Class, out: PrintStream): Unit = Compiler.compile(node, out)
}
