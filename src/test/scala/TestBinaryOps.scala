import org.junit.Test

import ast._
import ast.ImplicitConversions._

import MainTest._

class TestBinaryOps {

    val NaN = Float.NaN

    @Test
    def testAddInt() =
        checkOutput(Class(
            "testAddInt",
            Print(Add(52, 21))), (52 + 21).toString)

    @Test
    def testSubtractInt() =
        checkOutput(Class(
            "testSubtractInt",
            Print(Subtract(52, 21))), (52 - 21).toString)

    @Test
    def testMultInt() =
        checkOutput(Class(
            "testMultInt",
            Print(Multiply(76, 64))), (76 * 64).toString)

    @Test
    def testDivInt() =
        checkOutput(Class(
            "testDivInt",
            Print(Divide(76, 64))), (76 / 64).toString)

    @Test
    def testLessThanInt() =
        checkOutput(Class(
            "testLessThanInt",
            Print(LessThan(54, 63)),
            Print(LessThan(54, 54)),
            Print(LessThan(54, 21))), "true\nfalse\nfalse")

    @Test
    def testLessOrEqualInt() =
        checkOutput(Class(
            "testLessOrEqualInt",
            Print(LessOrEqual(54, 63)),
            Print(LessOrEqual(54, 54)),
            Print(LessOrEqual(54, 21))), "true\ntrue\nfalse")

    @Test
    def testEqualInt() =
        checkOutput(Class(
            "testEqualInt",
            Print(Equal(54, 63)),
            Print(Equal(54, 54)),
            Print(Equal(54, 21))), "false\ntrue\nfalse")

    @Test
    def testAddFloat() =
        checkOutput(Class(
            "testAddFloat",
            Print(Add(52.7f, 21.2f))), (52.7f + 21.2f).toString)

    @Test
    def testSubtractFloat() =
        checkOutput(Class(
            "testSubtractFloat",
            Print(Subtract(52.7f, 21.2f))), (52.7f - 21.2f).toString)

    @Test
    def testMultFloat() =
        checkOutput(Class(
            "testMultFloat",
            Print(Multiply(76.1f, 64.9f))), (76.1f * 64.9f).toString)

    @Test
    def testDivFloat() =
        checkOutput(Class(
            "testDivFloat",
            Print(Divide(76.1f, 64.9f))), (76.1f / 64.9f).toString)

    @Test
    def testLessThanFloat() =
        checkOutput(Class(
            "testLessThanFloat",
            Print(LessThan(54.7f, 63.2f)),
            Print(LessThan(54.7f, 54.7f)),
            Print(LessThan(54.7f, 21.2f))), "true\nfalse\nfalse")

    @Test
    def testLessOrEqualFloat() =
        checkOutput(Class(
            "testLessOrEqualFloat",
            Print(LessOrEqual(54.7f, 63.0f)),
            Print(LessOrEqual(54.7f, 54.7f)),
            Print(LessOrEqual(54.7f, 21.5f))), "true\ntrue\nfalse")

    @Test
    def testEqualFloat() =
        checkOutput(Class(
            "testEqualFloat",
            Print(Equal(54.7f, 63.5f)),
            Print(Equal(54.7f, 54.7f)),
            Print(Equal(54.7f, 21.1f))), "false\ntrue\nfalse")

    @Test
    def testAddIntFloat() =
        checkOutput(Class(
            "testAddIntFloat",
            Print(Add(52, 21.2f))), (52 + 21.2f).toString)

    @Test
    def testSubtractIntFloat() =
        checkOutput(Class(
            "testSubtractIntFloat",
            Print(Subtract(52, 21.2f))), (52 - 21.2f).toString)

    @Test
    def testMultIntFloat() =
        checkOutput(Class(
            "testMultIntFloat",
            Print(Multiply(76, 64.9f))), (76 * 64.9f).toString)

    @Test
    def testDivIntFloat() =
        checkOutput(Class(
            "testDivIntFloat",
            Print(Divide(76, 64.9f))), (76 / 64.9f).toString)

    @Test
    def testLessThanIntFloat() =
        checkOutput(Class(
            "testLessThanIntFloat",
            Print(LessThan(54, 63.6f)),
            Print(LessThan(54, 54.0f)),
            Print(LessThan(54, 21.1f))), "true\nfalse\nfalse")

    @Test
    def testLessOrEqualIntFloat() =
        checkOutput(Class(
            "testLessOrEqualIntFloat",
            Print(LessOrEqual(54, 63.1f)),
            Print(LessOrEqual(54, 54.0f)),
            Print(LessOrEqual(54, 21.3f))), "true\ntrue\nfalse")

    @Test
    def testEqualIntFloat() =
        checkOutput(Class(
            "testEqualIntFloat",
            Print(Equal(54, 63.2f)),
            Print(Equal(54, 54.0f)),
            Print(Equal(54, 21.8f))), "false\ntrue\nfalse")

    @Test
    def testAddFloatInt() =
        checkOutput(Class(
            "testAddFloatInt",
            Print(Add(52.7f, 21))), (52.7f + 21).toString)

    @Test
    def testSubtractFloatInt() =
        checkOutput(Class(
            "testSubtractFloatInt",
            Print(Subtract(52.7f, 21))), (52.7f - 21).toString)

    @Test
    def testMultFloatInt() =
        checkOutput(Class(
            "testMultFloatInt",
            Print(Multiply(76.1f, 64))), (76.1f * 64).toString)

    @Test
    def testDivFloatInt() =
        checkOutput(Class(
            "testDivFloatInt",
            Print(Divide(76.1f, 64))), (76.1f / 64).toString)

    @Test
    def testLessThanFloatInt() =
        checkOutput(Class(
            "testLessThanFloatInt",
            Print(LessThan(54.7f, 63)),
            Print(LessThan(54.0f, 54)),
            Print(LessThan(54.7f, 21))), "true\nfalse\nfalse")

    @Test
    def testLessOrEqualFloatInt() =
        checkOutput(Class(
            "testLessOrEqualFloatInt",
            Print(LessOrEqual(54.7f, 63)),
            Print(LessOrEqual(54.0f, 54)),
            Print(LessOrEqual(54.7f, 21))), "true\ntrue\nfalse")

    @Test
    def testEqualFloatInt() =
        checkOutput(Class(
            "testEqualFloatInt",
            Print(Equal(54.7f, 63)),
            Print(Equal(54.0f, 54)),
            Print(Equal(54.7f, 21))), "false\ntrue\nfalse")

    @Test
    def testAddNaN() =
        checkOutput(Class(
            "testAddNaN",
            Print(Add(NaN, NaN))), (NaN + NaN).toString)

    @Test
    def testSubtractNaN() =
        checkOutput(Class(
            "testSubtractNaN",
            Print(Subtract(NaN, NaN))), (NaN - NaN).toString)

    @Test
    def testMultNaN() =
        checkOutput(Class(
            "testMultNaN",
            Print(Multiply(NaN, NaN))), (NaN * NaN).toString)

    @Test
    def testDivNaN() =
        checkOutput(Class(
            "testDivNaN",
            Print(Divide(NaN, NaN))), (NaN / NaN).toString)

    @Test
    def testLessThanNaN() =
        checkOutput(Class(
            "testLessThanNaN",
            Print(LessThan(NaN, NaN))), (NaN < NaN).toString)

    @Test
    def testLessOrEqualNaN() =
        checkOutput(Class(
            "testLessOrEqualNaN",
            Print(LessOrEqual(NaN, NaN))), (NaN <= NaN).toString)

    @Test
    def testEqualNaN() =
        checkOutput(Class(
            "testEqualNaN",
            Print(Equal(NaN, NaN))), (NaN == NaN).toString)

    @Test
    def testAddIntNaN() =
        checkOutput(Class(
            "testAddIntNaN",
            Print(Add(52, NaN))), (52 + NaN).toString)

    @Test
    def testSubtractIntNaN() =
        checkOutput(Class(
            "testSubtractIntNaN",
            Print(Subtract(52, NaN))), (52 - NaN).toString)

    @Test
    def testMultIntNaN() =
        checkOutput(Class(
            "testMultIntNaN",
            Print(Multiply(76, NaN))), (76 * NaN).toString)

    @Test
    def testDivIntNaN() =
        checkOutput(Class(
            "testDivIntNaN",
            Print(Divide(76, NaN))), (76 / NaN).toString)

    @Test
    def testLessThanIntNaN() =
        checkOutput(Class(
            "testLessThanIntNaN",
            Print(LessThan(54, NaN))), (54 < NaN).toString)

    @Test
    def testLessOrEqualIntNaN() =
        checkOutput(Class(
            "testLessOrEqualIntNaN",
            Print(LessOrEqual(54, NaN))), (54 <= NaN).toString)

    @Test
    def testEqualIntNaN() =
        checkOutput(Class(
            "testEqualIntNaN",
            Print(Equal(54, NaN))), (54 == NaN).toString)

    @Test
    def testAddNaNInt() =
        checkOutput(Class(
            "testAddNaNInt",
            Print(Add(NaN, 21))), (NaN + 21).toString)

    @Test
    def testSubtractNaNInt() =
        checkOutput(Class(
            "testSubtractNaNInt",
            Print(Subtract(NaN, 21))), (NaN - 21).toString)

    @Test
    def testMultNaNInt() =
        checkOutput(Class(
            "testMultNaNInt",
            Print(Multiply(NaN, 64))), (NaN * 64).toString)

    @Test
    def testDivNaNInt() =
        checkOutput(Class(
            "testDivNaNInt",
            Print(Divide(NaN, 64))), (NaN / 64).toString)

    @Test
    def testLessThanNaNInt() =
        checkOutput(Class(
            "testLessThanNaNInt",
            Print(LessThan(NaN, 63))), (NaN < 63).toString)

    @Test
    def testLessOrEqualNaNInt() =
        checkOutput(Class(
            "testLessOrEqualNaNInt",
            Print(LessOrEqual(NaN, 63))), (NaN <= 63).toString)

    @Test
    def testEqualNaNInt() =
        checkOutput(Class(
            "testEqualNaNInt",
            Print(Equal(NaN, 63))), (NaN == 63).toString)

    @Test
    def testEqualBool() =
        checkOutput(Class(
            "testEqualBool",
            Print(Equal(true, true)),
            Print(Equal(true, false)),
            Print(Equal(false, true)),
            Print(Equal(false, false))),
            "true\nfalse\nfalse\ntrue")
}
