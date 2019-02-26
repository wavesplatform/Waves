package com.wavesplatform.transaction.smart.script

import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.ScriptEstimator
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.UserFunction
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.testing.TypedScriptGen
import com.wavesplatform.lang.{Global, StdLibVersion}
import com.wavesplatform.transaction.smart.WavesEnvironment
import com.wavesplatform.utils
import com.wavesplatform.utils.EmptyBlockchain
import monix.eval.Coeval
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class UserFunctionComplexityTest extends PropSpec with PropertyChecks with Matchers with TypedScriptGen {

  private val version = StdLibVersion.V3

  private val ctx = {
    utils.functionCosts(StdLibVersion.V3)
    Monoid
      .combineAll(
        Seq(
          PureContext.build(version),
          CryptoContext.build(Global),
          WavesContext.build(
            version,
            new WavesEnvironment('T'.toByte, Coeval(???), Coeval(???), EmptyBlockchain),
            isTokenContext = false
          )
        ))
  }

  private val funcCosts = utils.functionCosts(version)

  private def estimate(expr: EXPR): Either[String, Long] = {
    ScriptEstimator(ctx.evaluationContext.letDefs.keySet, funcCosts, expr)
  }

  // If test fails than complexity of user function was changed and it could lead to fork.
  property("WARNING - NODE FORK - check if user functions complexity changed") {
    val userFuncs = ctx.functions.filter(_.isInstanceOf[UserFunction])
    userFuncs.foreach {
      case func: UserFunction =>
        import func.signature.args
        val complexity =
          Coeval.now(ScriptEstimator(ctx.evaluationContext.letDefs.keySet ++ args.map(_._1), funcCosts, func.ev).explicitGet() + args.size * 5).value
        if (complexity != func.cost) {
          fail(s"Complexity of ${func.name} should be ${func.cost}, actual: $complexity.")
        }
      case _ =>
    }
  }

  property("estimate script with UserFunctions") {
    val exprNe = FUNCTION_CALL(PureContext.ne, List(CONST_LONG(1), CONST_LONG(2)))
    estimate(exprNe).explicitGet() shouldBe 28

    val exprThrow = FUNCTION_CALL(PureContext.throwNoMessage, List())
    estimate(exprThrow).explicitGet() shouldBe 2

    val exprExtract = LET_BLOCK(
      LET("x", CONST_LONG(2)),
      FUNCTION_CALL(PureContext.extract, List(REF("x")))
    )
    estimate(exprExtract).explicitGet() shouldBe 21

    val exprIsDefined = LET_BLOCK(
      LET("x", CONST_LONG(2)),
      FUNCTION_CALL(PureContext.isDefined, List(REF("x")))
    )
    estimate(exprIsDefined).explicitGet() shouldBe 43

    val exprDropRightBytes = FUNCTION_CALL(PureContext.dropRightBytes, List(CONST_BYTESTR(ByteStr.fromLong(2)), CONST_LONG(1)))
    estimate(exprDropRightBytes).explicitGet() shouldBe 21

    val exprTakeRightBytes = FUNCTION_CALL(PureContext.takeRightBytes, List(CONST_BYTESTR(ByteStr.fromLong(2)), CONST_LONG(1)))
    estimate(exprTakeRightBytes).explicitGet() shouldBe 21

    val exprDropRightString = FUNCTION_CALL(PureContext.dropRightString, List(CONST_STRING("str"), CONST_LONG(1)))
    estimate(exprDropRightString).explicitGet() shouldBe 21

    val exprTakeRightString = FUNCTION_CALL(PureContext.takeRightString, List(CONST_STRING("str"), CONST_LONG(1)))
    estimate(exprTakeRightString).explicitGet() shouldBe 21

    val exprUMinus = FUNCTION_CALL(PureContext.uMinus, List(CONST_LONG(1)))
    estimate(exprUMinus).explicitGet() shouldBe 10

    val exprUNot = FUNCTION_CALL(PureContext.uNot, List(TRUE))
    estimate(exprUNot).explicitGet() shouldBe 12

    val exprEnsure = FUNCTION_CALL(PureContext.ensure, List(TRUE))
    estimate(exprEnsure).explicitGet() shouldBe 17

    val exprDataByIndex = LET_BLOCK(
      LET("arr", FUNCTION_CALL(PureContext.listConstructor, List(CONST_STRING("str_1"), REF("nil")))),
      FUNCTION_CALL(User("getString"), List(REF("arr"), CONST_LONG(0)))
    )
    estimate(exprDataByIndex).explicitGet() shouldBe 43

    val exprAddressFromPublicKey = FUNCTION_CALL(User("addressFromPublicKey"), List(CONST_BYTESTR(ByteStr.fromLong(2))))
    estimate(exprAddressFromPublicKey).explicitGet() shouldBe 83

    val exprAddressFromString = FUNCTION_CALL(User("addressFromString"), List(CONST_STRING("address")))
    estimate(exprAddressFromString).explicitGet() shouldBe 125

    val exprWavesBalance = FUNCTION_CALL(User("wavesBalance"), List(CONST_STRING("alias")))
    estimate(exprWavesBalance).explicitGet() shouldBe 110
  }
}
