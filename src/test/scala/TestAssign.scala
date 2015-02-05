import org.junit.Test

import ast._
import ast.ImplicitConversions._

import MainTest._

class TestAssign {

    @Test
    def testAssign() =
        checkOutput(Class("testAssign", New("x", 42)), "")

    @Test
    def testPrintAssign() =
        checkOutput(Class("testPrintAssign", New("x", 42), Print("x")), "42")

    @Test
    def testAssignType() =
        checkOutput(Class("testAssignType", Print(New("x", 43))), "43")

    @Test
    def testAssignBool() =
        checkOutput(Class("testAssignBool", New("x", false), Print("x")), "false")

    @Test
    def testAssignFloat() =
        checkOutput(Class("testAssignFloat", New("x", 47.6f), Print("x")), "47.6")

    @Test
    def testAssignMultiple() =
        checkOutput(
            Class(
                "testAssignMultiple",
                New("x", 89),
                Print("x"),
                New("y", 67.2f),
                Print("y"),
                Print("x")),
            "89\n67.2\n89")

    @Test
    def testReassign() =
        checkOutput(
            Class(
                "testReassign",
                New("x", 87),
                Print("x"),
                Assign("x", 13),
                Print("x")),
            "87\n13")

    @Test
    def testPrintReassign() =
        checkOutput(
            Class(
                "testPrintReassign",
                New("x", 87),
                Print("x"),
                Print(Assign("x", 13))),
            "87\n13")

    @Test
    def testScope() =
        checkOutput(
            Class(
                "testScope",
                New("x", true),
                Stats(
                    New("y", false),
                    Print("x"),
                    Print("y")),
                New("y", 4)),
            "true\nfalse")

    @Test
    def testAssignScope() =
        checkOutput(
            Class(
                "testScope",
                New("x", true),
                Stats(
                    Assign("x", false),
                    Print("x")),
                Print("x")),
            "false\nfalse")
}
