package compiler.compilefunction

import compiler._

case class BoolConstant(constant: ast.BoolConstant) extends ReturnCompiler {
    /*
     * iconst_1 //true
     */
    override def ret(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) = {
        val asInt = if (constant.value) 1 else 0
        (CompileResult(Seq("iconst_" + asInt), labelsUsed, 0, 1), CFBool)
    }
}

case class FloatConstant(constant: ast.FloatConstant) extends ReturnCompiler {
    /*
     * ldc 6.543
     */
    override def ret(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) =
        (CompileResult(Seq("ldc " + constant.value), labelsUsed, 0, 1), CFFloat)
}

case class IntConstant(constant: ast.IntConstant) extends ReturnCompiler {
    override def ret(vars: Map[ast.Id, (CFType, Int)], gotos: Map[ast.Id, Goto], labelsUsed: Int, localsInUse: Int, line: Option[ast.Line]) = {
        /*
         * ldc 3301
         * OR
         * bipush 13
         */
        val instruction = if (-128 <= constant.value && constant.value <= 127) "bipush " else "ldc "
        (CompileResult(Seq(instruction + constant.value), labelsUsed, 0, 1), CFInt)
    }
}
