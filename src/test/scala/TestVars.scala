import org.junit.Test

import ast._
import ast.ImplicitConversions._

import MainTest._

class TestVars {
    @Test
    def testClass() =
        checkOutput(Class(Id("testEmpty")), "")

    @Test
    def testPrintInt() =
        checkOutput(Class("testPrintIntr", Print(31)), "31")

    @Test
    def testPrintFloat() =
        checkOutput(Class("testPrintFloat", Print(31.1f)), "31.1")

    @Test
    def testPrintBool() =
        checkOutput(Class("testPrintBool", Print(false)), "false")

    @Test
    def testPrintMultipleInt() =
        checkOutput(Class(
            "testPrintMultipleInt",
            Print(43),
            Print(42)), "43\n42")

    @Test
    def testPrintMultiple() =
        checkOutput(Class(
            "testPrintMultiple",
            Print(false),
            Print(42)), "false\n42")

    @Test
    def testLargeInt() =
        checkOutput(Class(
            "testLargeInt",
            Print(344354)), "344354")

}
