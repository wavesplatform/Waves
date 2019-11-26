package com.wavesplatform.lang.compiler

import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, ContentType, Expression, V3}
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ExpressionCompiler}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{Types, WavesContext}
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.parser.Expressions._
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ExpressionCompilerWithParserV2Test extends PropSpec with PropertyChecks with Matchers with NoShrink {

  def compile(script: String): Either[String, Expressions.EXPR] = {
    val ctx = Monoid.combine(
      compilerContext,
      WavesContext
        .build(
          DirectiveSet(V3, Account, Expression).explicitGet()
        )
        .compilerContext
    )
    val compResult = ExpressionCompiler.compileWithParseResult(script, ctx, false)
    compResult.map(_._2.expr)
  }

  property("simple test") {
    val script = """
      |{-# STDLIB_VERSION 3 #-}
      |{-# CONTENT_TYPE EXPRESSION #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |
      |let foo = 1234567
      |let bar = 987654
      |
      |if (foo + bar > 123456) then
      |   true
      |else
      |   false
      |
      |""".stripMargin

    val result = compile(script)

    result shouldBe 'right
    result.explicitGet() shouldBe
      BLOCK(
        AnyPos,
        LET(AnyPos, PART.VALID(AnyPos, "foo"), CONST_LONG(AnyPos, 1234567, Some(LONG), None), List(), false),
        BLOCK(
          AnyPos,
          LET(AnyPos, PART.VALID(AnyPos, "bar"), CONST_LONG(AnyPos, 987654, Some(LONG), None), List(), false),
          IF(
            AnyPos,
            FUNCTION_CALL(
              AnyPos,
              PART.VALID(AnyPos, ">"),
              List(
                FUNCTION_CALL(
                  AnyPos,
                  PART.VALID(AnyPos, "+"),
                  List(REF(AnyPos, PART.VALID(AnyPos, "foo"), Some(LONG), None), REF(AnyPos, PART.VALID(AnyPos, "bar"), Some(LONG), None)),
                  Some(LONG),
                  None
                ),
                CONST_LONG(AnyPos, 123456, Some(LONG), None)
              ),
              Some(BOOLEAN),
              None
            ),
            TRUE(AnyPos, Some(BOOLEAN), None),
            FALSE(AnyPos, Some(BOOLEAN), None),
            Some(BOOLEAN),
            None
          ),
          Some(BOOLEAN),
          None
        ),
        Some(BOOLEAN),
        None
      )
  }

  /*property("simple test 2") {
    val script = """
                   |{-# STDLIB_VERSION 3 #-}
                   |{-# CONTENT_TYPE EXPRESSION #-}
                   |{-# SCRIPT_TYPE ACCOUNT #-}
                   |
                   |let str = getString(this, "key")
                   |let num = 13579
                   |let list = [base58'', base58'', base58'']
                   |
                   |match str {
                   |  case s: String => true
                   |  case _ => false
                   |}
                   |""".stripMargin

    val result = compile(script)

    result shouldBe 'right
    result.explicitGet() shouldBe
      BLOCK(
        AnyPos,
        LET(
          AnyPos,
          PART.VALID(AnyPos, "str"),
          FUNCTION_CALL(
            AnyPos,
            PART.VALID(AnyPos, "getString"),
            List(REF(AnyPos, PART.VALID(AnyPos, "this"), Some(Types.addressType), None), CONST_STRING(AnyPos, PART.VALID(AnyPos, "key"), Some(STRING), None)),
            Some(UNION(List(STRING, UNIT))),
            None
          ),
          List(),
          false
        ),
        BLOCK(
          AnyPos,
          LET(AnyPos, PART.VALID(AnyPos, "num"), CONST_LONG(AnyPos, 13579, Some(LONG), None), List(), false),
          BLOCK(
            AnyPos,
            LET(
              AnyPos,
              PART.VALID(AnyPos, "list"),
              FUNCTION_CALL(
                AnyPos,
                PART.VALID(AnyPos, "cons"),
                List(
                  CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr(Array())), Some(BYTESTR), None),
                  FUNCTION_CALL(
                    AnyPos,
                    PART.VALID(AnyPos, "cons"),
                    List(
                      CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr(Array())), Some(BYTESTR), None),
                      FUNCTION_CALL(
                        AnyPos,
                        PART.VALID(AnyPos, "cons"),
                        List(
                          CONST_BYTESTR(AnyPos, PART.VALID(AnyPos, ByteStr(Array())), Some(BYTESTR), None),
                          REF(AnyPos, PART.VALID(AnyPos, "nil"), Some(LIST(NOTHING)), None)
                        ),
                        Some(LIST(BYTESTR)),
                        None
                      )
                    ),
                    Some(LIST(BYTESTR)),
                    None
                  )
                ),
                Some(LIST(BYTESTR)),
                None
              ),
              List(),
              false
            ),
            BLOCK(
              AnyPos,
              LET(
                AnyPos,
                PART.VALID(AnyPos, "$match0"),
                REF(AnyPos, PART.VALID(AnyPos, "str"), Some(UNION(List(STRING, UNIT))), None),
                List(),
                false
              ),
              IF(
                AnyPos,
                FUNCTION_CALL(
                  AnyPos,
                  PART.VALID(AnyPos, "_isInstanceOf"),
                  List(
                    REF(AnyPos, PART.VALID(AnyPos, "$match0"), Some(UNION(List(STRING, UNIT))), None),
                    CONST_STRING(AnyPos, PART.VALID(AnyPos, "String"), Some(STRING), None)
                  ),
                  Some(BOOLEAN),
                  None
                ),
                BLOCK(
                  AnyPos,
                  LET(
                    AnyPos,
                    PART.VALID(AnyPos, "s"),
                    REF(AnyPos, PART.VALID(AnyPos, "$match0"), Some(UNION(List(STRING, UNIT))), None),
                    Vector(PART.VALID(AnyPos, "String")),
                    false
                  ),
                  TRUE(AnyPos, Some(BOOLEAN), None),
                  Some(BOOLEAN),
                  None
                ),
                FALSE(AnyPos, Some(BOOLEAN), None),
                Some(BOOLEAN),
                None
              ),
              Some(BOOLEAN),
              None
            ),
            Some(BOOLEAN),
            None
          ),
          Some(BOOLEAN),
          None
        ),
        Some(BOOLEAN),
        None
      )
  }*/

}
