import MainTest._
import ast.ImplicitConversions._
import ast._
import org.junit.Test

class TestBranch {
    @Test(timeout = 5000)
    def testUnwhile() =
        checkOutput(Class(
            "testUnwhile",
            New("x", false),
            Unwhile(
                "x",
                Print("x"),
                Assign("x", true)),
            Print("x")), "false\ntrue")

    @Test(timeout = 5000)
    def testMultiUnwhile() =
        checkOutput(Class(
            "testMultiUnwhile",
            New("x", 3),
            Unwhile(
                Equal("x", -1),
                Print("x"),
                Assign("x", Subtract("x", 1)))
            ), "3\n2\n1\n0")

    @Test(timeout = 5000)
    def testNestedUnwhile() =
        checkOutput(Class(
            "testNestedUnwhile",
            New("i", 0),
            Unwhile(
                Not(LessThan("i", 3)),
                New("j", "i"),
                Unwhile(
                    Not(LessThan("j", 4)),
                    Print(Add("i", "j")),
                    Assign("j", Add("j", 1))
                ),
                Assign("i", Add("i", 1))
            )
        ), "0\n1\n2\n3\n2\n3\n4\n4\n5")

    @Test(timeout = 5000)
    def testComefrom() =
        checkOutput(Class(
            "testComefrom",
            Label("l"),
            Print(false),
            Comefrom("l"),
            Print(true)
        ), "true")

    @Test(timeout = 5000)
    def testMultiComefrom() =
        checkOutput(Class(
            "testMultiComefrom",
            Label("l"),
            Print(false),
            Comefrom("l"),
            Print(true),
            Label("m"),
            Print(false),
            Comefrom("m"),
            Print(true)
        ), "true\ntrue")
}
