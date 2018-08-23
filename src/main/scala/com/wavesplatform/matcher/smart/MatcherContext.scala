package com.wavesplatform.matcher.smart

import cats.data.EitherT
import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.compiler.Types.{FINAL, UNIT}
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings.orderObject
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
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

    def inaccessibleFunction(name: String, internalName: Short): BaseFunction =
      NativeFunction(name, 1, internalName, UNIT, Seq.empty: _*) {
        case _ =>
          s"Function ${name} is inaccessible when running script on matcher".asLeft
      }

    def inaccessibleUserFunction(name: String): BaseFunction = {
      NativeFunction(
        name,
        1,
        FunctionTypeSignature(UNIT, Seq.empty, FunctionHeader.User(name)),
        ev = {
          case _ =>
            s"Function ${name} is inaccessible when running script on matcher".asLeft
        }
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
