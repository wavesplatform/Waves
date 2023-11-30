package com.wavesplatform.lang.parser

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.{Expression, V3, V5}
import com.wavesplatform.lang.utils.*
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.{Decompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{GlobalValNames, PureContext}
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset.NoLibraries
import com.wavesplatform.test.{PropSpec, *}

class UnderscoreTest extends PropSpec {
  private def compile(script: String): Either[String, EXPR] =
    ExpressionCompiler.compile(script, NoLibraries, compilerContext(V5, Expression, false), V5).map(_._1)

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
    assert("__aa_a_a_", "_ff_f_f_", "_xx_x_x_") should produce("expected not more than 1 underscore in a row in 6-15")
    assert("_aa__a_a_", "_ff_f_f_", "_xx_x_x_") should produce("expected not more than 1 underscore in a row in 9-15")
    assert("_aa_a_a__", "_ff_f_f_", "_xx_x_x_") should produce("expected not more than 1 underscore in a row in 13-15")
    assert("_aa_a_a_", "__ff_f_f_", "_xx_x_x_") should produce("expected not more than 1 underscore in a row in 25-44")
    assert("_aa_a_a_", "_ff__f_f_", "_xx_x_x_") should produce("expected not more than 1 underscore in a row in 28-44")
    assert("_aa_a_a_", "_ff_f_f__", "_xx_x_x_") should produce("expected not more than 1 underscore in a row in 32-44")
    assert("_aa_a_a_", "_ff_f_f_", "__xx_x_x_") should produce("expected not more than 1 underscore in a row in 34-44")
    assert("_aa_a_a_", "_ff_f_f_", "_xx__x_x_") should produce("expected not more than 1 underscore in a row in 37-44")
    assert("_aa_a_a_", "_ff_f_f_", "_xx_x_x__") should produce("expected not more than 1 underscore in a row in 41-44")
  }

  property("internal functions can't be used directly") {
    compile(s" $$Tuple2(1, 1) ") should produce("can't parse the expression")
    compile(s""" $$isInstanceOf(1, "Int") """) should produce("can't parse the expression")
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
        LET(s"$$match0", CONST_LONG(1)),
        IF(
          FUNCTION_CALL(Native(1), List(REF(s"$$match0"), CONST_STRING("Int").explicitGet())), // Native(1) is isInstanceOf
          CONST_BOOLEAN(true),
          FUNCTION_CALL(Native(2), List(CONST_STRING("Match error").explicitGet()))
        )
      )
    )
  }

  property("FOLD with underscores") {
    compile(
      s"""
         | func _s_u_m_(_a_a_:Int, _b_b_:Int) = _a_a_ + _b_b_
         | let _a_r_r_ = [1, 2, 3, 4, 5]
         | FOLD<1>(_a_r_r_, 9, _s_u_m_)
      """.stripMargin
    ) shouldBe Right(
      BLOCK(
        FUNC("_s_u_m_", List("_a_a_", "_b_b_"), FUNCTION_CALL(Native(100), List(REF("_a_a_"), REF("_b_b_")))),
        LET_BLOCK(
          LET(
            "_a_r_r_",
            FUNCTION_CALL(
              Native(1100),
              List(
                CONST_LONG(1),
                FUNCTION_CALL(
                  Native(1100),
                  List(
                    CONST_LONG(2),
                    FUNCTION_CALL(
                      Native(1100),
                      List(
                        CONST_LONG(3),
                        FUNCTION_CALL(Native(1100), List(CONST_LONG(4), FUNCTION_CALL(Native(1100), List(CONST_LONG(5), REF(GlobalValNames.Nil)))))
                      )
                    )
                  )
                )
              )
            )
          ),
          BLOCK(
            LET(s"$$l", REF("_a_r_r_")),
            BLOCK(
              LET(s"$$s", FUNCTION_CALL(Native(400), List(REF(s"$$l")))),
              BLOCK(
                LET(s"$$acc0", CONST_LONG(9)),
                BLOCK(
                  FUNC(
                    "$f0_1",
                    List(s"$$a", s"$$i"),
                    IF(
                      FUNCTION_CALL(Native(103), List(REF(s"$$i"), REF(s"$$s"))),
                      REF(s"$$a"),
                      FUNCTION_CALL(User("_s_u_m_"), List(REF(s"$$a"), FUNCTION_CALL(Native(401), List(REF(s"$$l"), REF(s"$$i")))))
                    )
                  ),
                  BLOCK(
                    FUNC(
                      "$f0_2",
                      List(s"$$a", s"$$i"),
                      IF(
                        FUNCTION_CALL(Native(103), List(REF(s"$$i"), REF(s"$$s"))),
                        REF(s"$$a"),
                        FUNCTION_CALL(Native(2), List(CONST_STRING("List size exceeds 1").explicitGet()))
                      )
                    ),
                    FUNCTION_CALL(User(s"$$f0_2"), List(FUNCTION_CALL(User(s"$$f0_1"), List(REF(s"$$acc0"), CONST_LONG(0))), CONST_LONG(1)))
                  )
                )
              )
            )
          )
        )
      )
    )
  }
}
