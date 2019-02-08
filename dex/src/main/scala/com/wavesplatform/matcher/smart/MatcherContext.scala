package com.wavesplatform.matcher.smart

import cats.data.EitherT
import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.StdLibVersion._
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, CaseObj}
import com.wavesplatform.lang.v1.compiler.Types.{FINAL, UNIT}
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings.{ordType, orderObject}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.domain.OrdType
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import com.wavesplatform.lang.{Global, StdLibVersion}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.RealTransactionWrapper
import monix.eval.Coeval

object MatcherContext {

  def build(version: StdLibVersion, nByte: Byte, in: Coeval[Order], proofsEnabled: Boolean): EvaluationContext = {
    val baseContext = Monoid.combine(PureContext.build(version), CryptoContext.build(Global)).evaluationContext

    val inputEntityCoeval: Coeval[Either[String, CaseObj]] =
      Coeval.defer(in.map(o => Right(orderObject(RealTransactionWrapper.ord(o), proofsEnabled))))

    val sellOrdTypeCoeval: Coeval[Either[String, CaseObj]] = Coeval(Right(ordType(OrdType.Sell)))
    val buyOrdTypeCoeval: Coeval[Either[String, CaseObj]]  = Coeval(Right(ordType(OrdType.Buy)))

    val heightCoeval: Coeval[Either[String, CONST_LONG]] = Coeval.evalOnce(Left("height is inaccessible when running script on matcher"))

    val orderType: CaseType = buildOrderType(proofsEnabled)
    val matcherTypes        = Seq(addressType, orderType, assetPairType)

    val txMap: Map[String, ((FINAL, String), LazyVal)] =
      if (version == V1 || version == V2)
        Map(("tx", ((orderType.typeRef, "Processing order"), LazyVal(EitherT(inputEntityCoeval)))))
      else Map.empty

    val commonMatcherVars: Map[String, ((FINAL, String), LazyVal)] = Map(
      ("height", ((com.wavesplatform.lang.v1.compiler.Types.LONG, "undefined height placeholder"), LazyVal(EitherT(heightCoeval)))),
      ("Sell", ((ordTypeType, "Sell OrderType"), LazyVal(EitherT(sellOrdTypeCoeval)))),
      ("Buy", ((ordTypeType, "Buy OrderType"), LazyVal(EitherT(buyOrdTypeCoeval))))
    )
    val matcherVars = commonMatcherVars ++ txMap

    def inaccessibleFunction(name: String, internalName: Short): BaseFunction = {
      val msg = s"Function $name is inaccessible when running script on matcher"
      NativeFunction(name, 1, internalName, UNIT, msg, Seq.empty: _*) { case _ => msg.asLeft }
    }

    def inaccessibleUserFunction(name: String): BaseFunction = {
      val msg = s"Function $name is inaccessible when running script on matcher"
      NativeFunction(
        name,
        StdLibVersion.SupportedVersions.map(_ -> 1L).toMap,
        FunctionTypeSignature(UNIT, Seq.empty, FunctionHeader.User(name)),
        _ => msg.asLeft,
        msg,
        Array.empty
      )
    }

    val getIntegerF: BaseFunction           = inaccessibleFunction("getInteger", DATA_LONG_FROM_STATE)
    val getBooleanF: BaseFunction           = inaccessibleFunction("getBoolean", DATA_BOOLEAN_FROM_STATE)
    val getBinaryF: BaseFunction            = inaccessibleFunction("getBinary", DATA_BYTES_FROM_STATE)
    val getStringF: BaseFunction            = inaccessibleFunction("getString", DATA_STRING_FROM_STATE)
    val txByIdF: BaseFunction               = inaccessibleFunction("txByIdF", GETTRANSACTIONBYID)
    val txHeightByIdF: BaseFunction         = inaccessibleFunction("txHeightByIdF", TRANSACTIONHEIGHTBYID)
    val addressFromPublicKeyF: BaseFunction = inaccessibleUserFunction("addressFromPublicKeyF")
    val addressFromStringF: BaseFunction    = inaccessibleUserFunction("addressFromStringF")
    val addressFromRecipientF: BaseFunction = inaccessibleFunction("addressFromRecipientF", ADDRESSFROMRECIPIENT)
    val assetBalanceF: BaseFunction         = inaccessibleFunction("assetBalanceF", ACCOUNTASSETBALANCE)
    val wavesBalanceF: BaseFunction         = inaccessibleUserFunction("wavesBalanceF")

    val functions = Array(
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
