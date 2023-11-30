package com.wavesplatform.lang.compiler

import cats.implicits.toBifunctorOps
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.directives.{Directive, DirectiveParser}
import com.wavesplatform.lang.utils
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.parser.Expressions.*
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset.NoLibraries
import com.wavesplatform.test.PropSpec

class ExpressionCompilerWithParserV2Test extends PropSpec {

  def compile(script: String, saveExprContext: Boolean = false): Either[String, Expressions.EXPR] = {

    val result = for {
      directives <- DirectiveParser(script)
      ds         <- Directive.extractDirectives(directives)
      ctx = utils.compilerContext(ds)
      compResult <- ExpressionCompiler
        .compileWithParseResult(script, NoLibraries, ctx, StdLibVersion.VersionDic.all.last, saveExprContext)
        .leftMap(_._1)
    } yield compResult

    result.map(_._2.expr)
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
                 """.stripMargin

    val result = compile(script)

    result shouldBe Symbol("right")
    result.explicitGet() shouldBe
      BLOCK(
        AnyPos,
        LET(AnyPos, PART.VALID(AnyPos, "foo"), CONST_LONG(AnyPos, 1234567, None)),
        BLOCK(
          AnyPos,
          LET(AnyPos, PART.VALID(AnyPos, "bar"), CONST_LONG(AnyPos, 987654, None)),
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
                CONST_LONG(AnyPos, 123456, None)
              ),
              Some(BOOLEAN),
              None
            ),
            TRUE(AnyPos, None),
            FALSE(AnyPos, None),
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

  property("simple test 2") {
    val script = """
                   |{-# STDLIB_VERSION 3 #-}
                   |{-# CONTENT_TYPE EXPRESSION #-}
                   |{-# SCRIPT_TYPE ACCOUNT #-}
                   |
                   |#define public keys
                   |let alicePubKey  = base58'5AzfA9UfpWVYiwFwvdr77k6LWupSTGLb14b24oVdEpMM'
                   |let bobPubKey    = base58'2KwU4vzdgPmKyf7q354H9kSyX9NZjNiq4qbnH2wi2VDF'
                   |let cooperPubKey = base58'GbrUeGaBfmyFJjSQb9Z8uTCej5GzjXfRDVGJGrmgt5cD'
                   |
                   |#check whoever provided the valid proof
                   |let aliceSigned  = if(sigVerify(tx.bodyBytes, tx.proofs[0], alicePubKey  )) then 1 else 0
                   |let bobSigned    = if(sigVerify(tx.bodyBytes, tx.proofs[1], bobPubKey    )) then 1 else 0
                   |func cooperSigned() = if(sigVerify(tx.bodyBytes, tx.proofs[2], cooperPubKey))then 1 else 0
                   |
                   |let n = tx.bodyBytes
                   |let a = sigVerify(tx.bodyBytes, tx.proofs[2], cooperPubKey)
                   |
                   |#sum up every valid proof to get at least 2
                   |aliceSigned + bobSigned + cooperSigned() >= 2
                   |
                   |""".stripMargin

    val result = compile(script, true)

    result shouldBe Symbol("right")
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

    result shouldBe Symbol("right")
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
                          REF(AnyPos, PART.VALID(AnyPos, GlobalValNames.Nil), Some(LIST(NOTHING)), None)
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
