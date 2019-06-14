package com.wavesplatform.matcher.smart

import cats.Eval
import cats.data.EitherT
import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.lang.{ExecutionError, Global}
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values._
import StdLibVersion.VersionDic
import com.wavesplatform.lang.v1.compiler.Terms.{CaseObj, EVALUATED}
import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, FINAL, LONG, UNIT}
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings.{ordType, orderObject}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.domain.OrdType
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.RealTransactionWrapper

object MatcherContext {

  def build(version: StdLibVersion, nByte: Byte, in: Eval[Order], proofsEnabled: Boolean): EvaluationContext = {
    val baseContext = Monoid.combine(PureContext.build(Global, version), CryptoContext.build(Global, version)).evaluationContext

    val inputEntityCoeval: Eval[Either[ExecutionError, CaseObj]] =
      Eval.defer(in.map(o => Right(orderObject(RealTransactionWrapper.ord(o), proofsEnabled, version))))

    def constVal[T <: EVALUATED](x: T): LazyVal = LazyVal(EitherT(Eval.now[Either[ExecutionError, T]](Right(x))))
    def inaccessibleVal(name: String): LazyVal =
      LazyVal(EitherT(Eval.now[Either[ExecutionError, Nothing]](Left(s"$name is inaccessible when running script on matcher"))))

    val orderType: CASETYPEREF = buildOrderType(proofsEnabled)
    val matcherTypes           = Seq(addressType, orderType, assetPairType)

    val vars: Map[String, ((FINAL, String), LazyVal)] = Seq[(String, FINAL, String, LazyVal)](
      ("tx", orderType, "Processing order", LazyVal(EitherT(inputEntityCoeval))),
      ("height", LONG, "undefined height placeholder", inaccessibleVal("height")),
      ("lastBlock", blockInfo, "undefined lastBlock placeholder", inaccessibleVal("lastBlock")),
      ("Sell", ordTypeType, "Sell OrderType", constVal(ordType(OrdType.Sell))),
      ("Buy", ordTypeType, "Buy OrderType", constVal(ordType(OrdType.Buy)))
    ).map { case (name, retType, desc, value) => (name, ((retType, desc), value)) }(collection.breakOut)

    def inaccessibleFunction(internalName: Short, name: String): BaseFunction = {
      val msg = s"Function $name is inaccessible when running script on matcher"
      NativeFunction(name, 1, internalName, UNIT, msg, Seq.empty: _*) { case _ => msg.asLeft }
    }

    def inaccessibleUserFunction(name: String): BaseFunction = {
      val msg = s"Function $name is inaccessible when running script on matcher"
      NativeFunction(
        name,
        DirectiveDictionary[StdLibVersion].all.map(_ -> 1L).toMap,
        FunctionTypeSignature(UNIT, Seq.empty, FunctionHeader.User(name)),
        _ => msg.asLeft,
        msg,
        Array.empty
      )
    }

    val nativeFunctions = Array(
      DATA_LONG_FROM_STATE    -> "getInteger",
      DATA_BOOLEAN_FROM_STATE -> "getBoolean",
      DATA_BYTES_FROM_STATE   -> "getBinary",
      DATA_STRING_FROM_STATE  -> "getString",
      GETTRANSACTIONBYID      -> "txById",
      ADDRESSFROMRECIPIENT    -> "addressFromRecipient",
      ACCOUNTASSETBALANCE     -> "assetBalance",
      GETASSETINFOBYID        -> "assetInfo",
      TRANSFERTRANSACTIONBYID -> "transferTransactionById",
      TRANSACTIONHEIGHTBYID   -> "transactionHeightById",
      BLOCKINFOBYHEIGHT       -> "blockInfoByHeight"
    ).map(Function.tupled(inaccessibleFunction))

    val userFunctions = Array(
      "getIntegerValue",
      "getBooleanValue",
      "getBinaryValue",
      "getStringValue",
      "addressFromPublicKey",
      "addressFromString",
      "wavesBalance"
    ).map(inaccessibleUserFunction)

    val matcherContext = CTX(matcherTypes, vars, nativeFunctions ++ userFunctions).evaluationContext

    baseContext |+| matcherContext
  }

}
