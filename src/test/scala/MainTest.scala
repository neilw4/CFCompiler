import java.io.{ ByteArrayOutputStream, PrintStream, File }
import java.net.URLClassLoader

import org.junit.Assert._

object MainTest {
    val testDirectory = "testOutput/"

    def createTestDirectory() = {
        new File(testDirectory).mkdir()
    }

    def compileToAssembly(node: ast.Class, output: File) = {
        output.delete()
        val printOut = new PrintStream(output)
        Compile.compile(node, printOut)
        printOut.flush()
    }

    def compileToBytecode(jFile: File, directory: String) = {
        jasmin.Main.main(Array("-d", directory, jFile.getAbsolutePath))
    }

    def runBytecode(classFile: File, className: String): String = {
        val directoryURL = classFile.getParentFile.toURI.toURL
        val loader = new URLClassLoader(Array(directoryURL))
        val bytecode = loader.loadClass(className)
        val mainMethod = bytecode.getMethod("main", Array[String]().getClass)
        val out = System.out
        val outToString = new ByteArrayOutputStream
        System.setOut(new PrintStream(outToString))
        mainMethod.invoke(null, null)
        System.setOut(out)
        val output = outToString.toString("UTF8")
        val printOutput = new PrintStream(classFile.getAbsolutePath.replace(".class", ".out"))
        printOutput.print(output)
        printOutput.close()
        output
    }

    def compileAndRun(node: ast.Class): String = {
        createTestDirectory()
        val className = node.id.name
        val jFile = new File(testDirectory + className + ".j")
        compileToAssembly(node, jFile)
        val classFile = new File(testDirectory + className + ".class")
        compileToBytecode(jFile, testDirectory)
        runBytecode(classFile, className)
    }

    def checkOutput(node: ast.Class, expected: String) = assertEquals(expected.trim, compileAndRun(node).trim)
}
