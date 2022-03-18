package com.wavesplatform.lang.compiler

import com.wavesplatform.lang.directives.values.{Expression, V3}
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{Decompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.test.{PropSpec, _}

class UnderscoreTest extends PropSpec {
  private def compile(script: String): Either[String, EXPR] =
    ExpressionCompiler.compile(script, compilerContext(V3, Expression, false)).map(_._1)

  private def assert(a: String, f: String, x: String): Either[String, EXPR] = {
    val script =
      s"""
         | let $a = 2
         | func $f($x: Int) = $x * $x
         | $f($a)
       """.stripMargin
    compile(script)
  }

  property("underscores in definitions are allowed") {
    val expected =
      LET_BLOCK(
        LET("_aa_a_a_", CONST_LONG(2)),
        BLOCK(
          FUNC("_ff_f_f_", List("_xx_x_x_"), FUNCTION_CALL(PureContext.mulLong.header, List(REF("_xx_x_x_"), REF("_xx_x_x_")))),
          FUNCTION_CALL(User("_ff_f_f_"), List(REF("_aa_a_a_")))
        )
      )
    assert("_aa_a_a_", "_ff_f_f_", "_xx_x_x_") shouldBe Right(expected)
    Decompiler(expected, getDecompilerContext(V3, Expression)) shouldBe
      """
        |let _aa_a_a_ = 2
        |func _ff_f_f_ (_xx_x_x_) = (_xx_x_x_ * _xx_x_x_)
        |
        |_ff_f_f_(_aa_a_a_)
      """.stripMargin.trim
  }

  property("two or more underscores in a row are prohibited") {
    assert("__aa_a_a_", "_ff_f_f_", "_xx_x_x_") should produce("Parsed.Failure")
    assert("_aa__a_a_", "_ff_f_f_", "_xx_x_x_") should produce("Parsed.Failure")
    assert("_aa_a_a__", "_ff_f_f_", "_xx_x_x_") should produce("Parsed.Failure")
    assert("_aa_a_a_", "__ff_f_f_", "_xx_x_x_") should produce("Parsed.Failure")
    assert("_aa_a_a_", "_ff__f_f_", "_xx_x_x_") should produce("Parsed.Failure")
    assert("_aa_a_a_", "_ff_f_f__", "_xx_x_x_") should produce("Parsed.Failure")
    assert("_aa_a_a_", "_ff_f_f_", "__xx_x_x_") should produce("Parsed.Failure")
    assert("_aa_a_a_", "_ff_f_f_", "_xx__x_x_") should produce("Parsed.Failure")
    assert("_aa_a_a_", "_ff_f_f_", "_xx_x_x__") should produce("Parsed.Failure")
  }

  property("internal functions can't be used by user") {
    compile(" %Tuple2(1, 1) ") should produce("can't parse the expression")
    compile(""" %isInstanceOf(1, "Int") """) should produce("can't parse the expression")
  }
}
