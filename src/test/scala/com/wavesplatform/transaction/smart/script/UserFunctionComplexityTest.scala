package com.wavesplatform.transaction.smart.script

import cats.kernel.Monoid
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.ScriptEstimator
import com.wavesplatform.lang.v1.evaluator.ctx.UserFunction
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
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

  // If test fails than complexity of user function was changed and it could lead to fork.
  property("WARNING - NODE FORK - check if user functions complexity changed") {
    val funcCosts = utils.functionCosts(version)

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
}
