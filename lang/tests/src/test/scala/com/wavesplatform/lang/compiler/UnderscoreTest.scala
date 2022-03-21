package com.wavesplatform.lang.compiler

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.{Expression, V3, V5}
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{Decompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.test.{PropSpec, _}

class UnderscoreTest extends PropSpec {
  private def compile(script: String): Either[String, EXPR] =
    ExpressionCompiler.compile(script, compilerContext(V5, Expression, false)).map(_._1)

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

  property("internal functions can't be used directly") {
    compile(" $Tuple2(1, 1) ") should produce("can't parse the expression")
    compile(""" $isInstanceOf(1, "Int") """) should produce("can't parse the expression")
  }

  property("internal functions names don't affect compiled expression") {
    compile("(1, 1)") shouldBe Right(FUNCTION_CALL(Native(1300), List(CONST_LONG(1), CONST_LONG(1))))
    compile(
      """ match 1 {
        |   case _: Int => true
        | }
      """.stripMargin
    ) shouldBe Right(
      LET_BLOCK(
        LET("$match0", CONST_LONG(1)),
        IF(
          FUNCTION_CALL(Native(1), List(REF("$match0"), CONST_STRING("Int").explicitGet())), // Native(1) is isInstanceOf
          CONST_BOOLEAN(true),
          FUNCTION_CALL(Native(2), List(CONST_STRING("Match error").explicitGet()))
        )
      ))
  }
}
