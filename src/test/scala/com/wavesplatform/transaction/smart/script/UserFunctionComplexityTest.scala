package com.wavesplatform.transaction.smart.script

import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.StdLibVersion.V2
import com.wavesplatform.lang.utils.DirectiveSet
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.{CTX, FunctionHeader, ScriptEstimator}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.testing.TypedScriptGen
import com.wavesplatform.lang.{ContentType, Global, ScriptType, StdLibVersion}
import com.wavesplatform.transaction.smart.WavesEnvironment
import com.wavesplatform.utils
import com.wavesplatform.utils.EmptyBlockchain
import monix.eval.Coeval
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class UserFunctionComplexityTest extends PropSpec with PropertyChecks with Matchers with TypedScriptGen {

  private def estimate(expr: EXPR, ctx: CTX, funcCosts: Map[FunctionHeader, Coeval[Long]]): Either[String, Long] = {
    ScriptEstimator(ctx.evaluationContext.letDefs.keySet, funcCosts, expr)
  }

  private val ctxV1 = {
    utils.functionCosts(StdLibVersion.V1)
    Monoid
      .combineAll(
        Seq(
          PureContext.build(StdLibVersion.V1),
          CryptoContext.build(Global),
          WavesContext.build(
            DirectiveSet(StdLibVersion.V1, ScriptType.Account, ContentType.Expression).explicitGet(),
            new WavesEnvironment('T'.toByte, Coeval(???), Coeval(???), EmptyBlockchain, Coeval(???)),
          )
        ))
  }
  private val funcCostsV1 = utils.functionCosts(StdLibVersion.V1)

  property("estimate script for stdLib V1 with UserFunctions") {

    def est: EXPR => Either[String, Long] = estimate(_, ctxV1, funcCostsV1)

    val exprNe = FUNCTION_CALL(PureContext.ne, List(CONST_LONG(1), CONST_LONG(2)))
    est(exprNe).explicitGet() shouldBe 28

    val exprThrow = FUNCTION_CALL(PureContext.throwNoMessage, List())
    est(exprThrow).explicitGet() shouldBe 2

    val exprExtract = LET_BLOCK(
      LET("x", CONST_LONG(2)),
      FUNCTION_CALL(PureContext.extract, List(REF("x")))
    )
    est(exprExtract).explicitGet() shouldBe 21

    val exprIsDefined = LET_BLOCK(
      LET("x", CONST_LONG(2)),
      FUNCTION_CALL(PureContext.isDefined, List(REF("x")))
    )
    est(exprIsDefined).explicitGet() shouldBe 43

    val exprDropRightBytes = FUNCTION_CALL(PureContext.dropRightBytes, List(CONST_BYTESTR(ByteStr.fromLong(2)), CONST_LONG(1)))
    est(exprDropRightBytes).explicitGet() shouldBe 21

    val exprTakeRightBytes = FUNCTION_CALL(PureContext.takeRightBytes, List(CONST_BYTESTR(ByteStr.fromLong(2)), CONST_LONG(1)))
    est(exprTakeRightBytes).explicitGet() shouldBe 21

    val exprDropRightString = FUNCTION_CALL(PureContext.dropRightString, List(CONST_STRING("str"), CONST_LONG(1)))
    est(exprDropRightString).explicitGet() shouldBe 21

    val exprTakeRightString = FUNCTION_CALL(PureContext.takeRightString, List(CONST_STRING("str"), CONST_LONG(1)))
    est(exprTakeRightString).explicitGet() shouldBe 21

    val exprUMinus = FUNCTION_CALL(PureContext.uMinus, List(CONST_LONG(1)))
    est(exprUMinus).explicitGet() shouldBe 10

    val exprUNot = FUNCTION_CALL(PureContext.uNot, List(TRUE))
    est(exprUNot).explicitGet() shouldBe 12

    val exprAddressFromPublicKey = FUNCTION_CALL(User("addressFromPublicKey"), List(CONST_BYTESTR(ByteStr.fromLong(2))))
    est(exprAddressFromPublicKey).explicitGet() shouldBe 83

    val exprAddressFromString = FUNCTION_CALL(User("addressFromString"), List(CONST_STRING("address")))
    est(exprAddressFromString).explicitGet() shouldBe 125

    val exprWavesBalance = FUNCTION_CALL(User("wavesBalance"), List(CONST_STRING("alias")))
    est(exprWavesBalance).explicitGet() shouldBe 110
  }

  private val ctxV2 = {
    utils.functionCosts(StdLibVersion.V2)
    Monoid
      .combineAll(
        Seq(
          PureContext.build(StdLibVersion.V2),
          CryptoContext.build(Global),
          WavesContext.build(
            DirectiveSet(V2, ScriptType.Account, ContentType.Expression).explicitGet(),
            new WavesEnvironment('T'.toByte, Coeval(???), Coeval(???), EmptyBlockchain, Coeval(???))
          )
        ))
  }
  private val funcCostsV2 = utils.functionCosts(StdLibVersion.V2)

  property("estimate script for stdLib V2 with UserFunctions") {

    def est: EXPR => Either[String, Long] = estimate(_, ctxV2, funcCostsV2)

    val exprNe = FUNCTION_CALL(PureContext.ne, List(CONST_LONG(1), CONST_LONG(2)))
    est(exprNe).explicitGet() shouldBe 28

    val exprThrow = FUNCTION_CALL(PureContext.throwNoMessage, List())
    est(exprThrow).explicitGet() shouldBe 2

    val exprExtract = LET_BLOCK(
      LET("x", CONST_LONG(2)),
      FUNCTION_CALL(PureContext.extract, List(REF("x")))
    )
    est(exprExtract).explicitGet() shouldBe 21

    val exprIsDefined = LET_BLOCK(
      LET("x", CONST_LONG(2)),
      FUNCTION_CALL(PureContext.isDefined, List(REF("x")))
    )
    est(exprIsDefined).explicitGet() shouldBe 43

    val exprDropRightBytes = FUNCTION_CALL(PureContext.dropRightBytes, List(CONST_BYTESTR(ByteStr.fromLong(2)), CONST_LONG(1)))
    est(exprDropRightBytes).explicitGet() shouldBe 21

    val exprTakeRightBytes = FUNCTION_CALL(PureContext.takeRightBytes, List(CONST_BYTESTR(ByteStr.fromLong(2)), CONST_LONG(1)))
    est(exprTakeRightBytes).explicitGet() shouldBe 21

    val exprDropRightString = FUNCTION_CALL(PureContext.dropRightString, List(CONST_STRING("str"), CONST_LONG(1)))
    est(exprDropRightString).explicitGet() shouldBe 21

    val exprTakeRightString = FUNCTION_CALL(PureContext.takeRightString, List(CONST_STRING("str"), CONST_LONG(1)))
    est(exprTakeRightString).explicitGet() shouldBe 21

    val exprUMinus = FUNCTION_CALL(PureContext.uMinus, List(CONST_LONG(1)))
    est(exprUMinus).explicitGet() shouldBe 10

    val exprUNot = FUNCTION_CALL(PureContext.uNot, List(TRUE))
    est(exprUNot).explicitGet() shouldBe 12

    val exprAddressFromPublicKey = FUNCTION_CALL(User("addressFromPublicKey"), List(CONST_BYTESTR(ByteStr.fromLong(2))))
    est(exprAddressFromPublicKey).explicitGet() shouldBe 83

    val exprAddressFromString = FUNCTION_CALL(User("addressFromString"), List(CONST_STRING("address")))
    est(exprAddressFromString).explicitGet() shouldBe 125

    val exprWavesBalance = FUNCTION_CALL(User("wavesBalance"), List(CONST_STRING("alias")))
    est(exprWavesBalance).explicitGet() shouldBe 110
  }

  private val ctxV3 = {
    utils.functionCosts(StdLibVersion.V3)
    Monoid
      .combineAll(
        Seq(
          PureContext.build(StdLibVersion.V3),
          CryptoContext.build(Global),
          WavesContext.build(
            DirectiveSet(StdLibVersion.V3, ScriptType.Account, ContentType.Expression).explicitGet(),
            new WavesEnvironment('T'.toByte, Coeval(???), Coeval(???), EmptyBlockchain, Coeval(???))
          )
        ))
  }
  private val funcCostsV3 = utils.functionCosts(StdLibVersion.V3)

  property("estimate script for stdLib V3 with UserFunctions") {

    def est: EXPR => Either[String, Long] = estimate(_, ctxV3, funcCostsV3)

    val exprNe = FUNCTION_CALL(PureContext.ne, List(CONST_LONG(1), CONST_LONG(2)))
    est(exprNe).explicitGet() shouldBe 3

    val exprThrow = FUNCTION_CALL(PureContext.throwNoMessage, List())
    est(exprThrow).explicitGet() shouldBe 1

    val exprExtract = LET_BLOCK(
      LET("x", CONST_LONG(2)),
      FUNCTION_CALL(PureContext.extract, List(REF("x")))
    )
    est(exprExtract).explicitGet() shouldBe 21

    val exprIsDefined = LET_BLOCK(
      LET("x", CONST_LONG(2)),
      FUNCTION_CALL(PureContext.isDefined, List(REF("x")))
    )
    est(exprIsDefined).explicitGet() shouldBe 9

    val exprDropRightBytes = FUNCTION_CALL(PureContext.dropRightBytes, List(CONST_BYTESTR(ByteStr.fromLong(2)), CONST_LONG(1)))
    est(exprDropRightBytes).explicitGet() shouldBe 21

    val exprTakeRightBytes = FUNCTION_CALL(PureContext.takeRightBytes, List(CONST_BYTESTR(ByteStr.fromLong(2)), CONST_LONG(1)))
    est(exprTakeRightBytes).explicitGet() shouldBe 21

    val exprDropRightString = FUNCTION_CALL(PureContext.dropRightString, List(CONST_STRING("str"), CONST_LONG(1)))
    est(exprDropRightString).explicitGet() shouldBe 21

    val exprTakeRightString = FUNCTION_CALL(PureContext.takeRightString, List(CONST_STRING("str"), CONST_LONG(1)))
    est(exprTakeRightString).explicitGet() shouldBe 21

    val exprUMinus = FUNCTION_CALL(PureContext.uMinus, List(CONST_LONG(1)))
    est(exprUMinus).explicitGet() shouldBe 2

    val exprUNot = FUNCTION_CALL(PureContext.uNot, List(TRUE))
    est(exprUNot).explicitGet() shouldBe 2

    val exprEnsure = FUNCTION_CALL(PureContext.ensure, List(TRUE))
    est(exprEnsure).explicitGet() shouldBe 17

    val exprDataByIndex = LET_BLOCK(
      LET("arr", FUNCTION_CALL(PureContext.listConstructor, List(CONST_STRING("str_1"), REF("nil")))),
      FUNCTION_CALL(User("getString"), List(REF("arr"), CONST_LONG(0)))
    )
    est(exprDataByIndex).explicitGet() shouldBe 43

    val exprAddressFromPublicKey = FUNCTION_CALL(User("addressFromPublicKey"), List(CONST_BYTESTR(ByteStr.fromLong(2))))
    est(exprAddressFromPublicKey).explicitGet() shouldBe 83

    val exprAddressFromString = FUNCTION_CALL(User("addressFromString"), List(CONST_STRING("address")))
    est(exprAddressFromString).explicitGet() shouldBe 125

    val exprWavesBalance = FUNCTION_CALL(User("wavesBalance"), List(CONST_STRING("alias")))
    est(exprWavesBalance).explicitGet() shouldBe 110
  }
}
