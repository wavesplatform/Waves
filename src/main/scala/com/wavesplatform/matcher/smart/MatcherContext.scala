package com.wavesplatform.matcher.smart

import cats.data.EitherT
import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Types.{FINAL, UNIT}
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings.orderObject
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.RealTransactionWrapper
import monix.eval.Coeval

object MatcherContext {

  private val baseContext = Monoid.combine(PureContext.ctx, CryptoContext.build(Global)).evaluationContext

  def build(nByte: Byte, in: Coeval[Order]): EvaluationContext = {
    val inputEntityCoeval: Coeval[Either[String, CaseObj]] =
      Coeval.defer(in.map(o => Right(orderObject(RealTransactionWrapper.ord(o)))))

    val heightCoeval: Coeval[Either[String, Long]] = Coeval.evalOnce(Left("height is inaccessible when running script on matcher"))

    val matcherTypes = Seq(addressType, aliasType, orderType, assetPairType)

    val matcherVars: Map[String, (FINAL, LazyVal)] = Map(
      ("height", (com.wavesplatform.lang.v1.compiler.Types.LONG, LazyVal(EitherT(heightCoeval)))),
      ("tx", (orderType.typeRef, LazyVal(EitherT(inputEntityCoeval))))
    )

    def inaccessibleFunction(name: String) =
      NativeFunction(name, 1, 1, UNIT, Seq.empty: _*) {
        case _ =>
          s"Function ${name} is inaccessible when running script on matcher".asLeft
      }

    val getIntegerF: BaseFunction           = inaccessibleFunction("getInteger")
    val getBooleanF: BaseFunction           = inaccessibleFunction("getBoolean")
    val getBinaryF: BaseFunction            = inaccessibleFunction("getBinary")
    val getStringF: BaseFunction            = inaccessibleFunction("getString")
    val txByIdF: BaseFunction               = inaccessibleFunction("txByIdF")
    val txHeightByIdF: BaseFunction         = inaccessibleFunction("txHeightByIdF")
    val addressFromPublicKeyF: BaseFunction = inaccessibleFunction("addressFromPublicKeyF")
    val addressFromStringF: BaseFunction    = inaccessibleFunction("addressFromStringF")
    val addressFromRecipientF: BaseFunction = inaccessibleFunction("addressFromRecipientF")
    val assetBalanceF: BaseFunction         = inaccessibleFunction("assetBalanceF")
    val wavesBalanceF: BaseFunction         = inaccessibleFunction("wavesBalanceF")

    val functions = Seq(
      txByIdF,
      txHeightByIdF,
      getIntegerF,
      getBooleanF,
      getBinaryF,
      getStringF,
      addressFromPublicKeyF,
      addressFromStringF,
      addressFromRecipientF,
      assetBalanceF,
      wavesBalanceF
    )

    val matcherContext = CTX(matcherTypes, matcherVars, functions).evaluationContext

    baseContext |+| matcherContext
  }

}
