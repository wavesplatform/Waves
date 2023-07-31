package com.wavesplatform.api.http.utils

import cats.syntax.either.*
import com.wavesplatform.account.{Address, AddressOrAlias, AddressScheme, PublicKey}
import com.wavesplatform.api.http.utils.UtilsApiRoute.{DefaultAddress, DefaultPublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invocation
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, ScriptExtraFee}
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionLike
import com.wavesplatform.state.{AccountScriptInfo, Blockchain}
import com.wavesplatform.transaction.TransactionType.{InvokeScript, TransactionType}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.{Asset, TransactionType}
import monix.eval.Coeval
import play.api.libs.json.JsObject

sealed trait Evaluation extends Product with Serializable {
  def blockchain: Blockchain
  def txLike: InvokeScriptTransactionLike
  def dAppToExpr(dApp: DApp): Either[ValidationError, EXPR]
}

object Evaluation {
  def build(blockchain: Blockchain, dAppAddress: Address, request: JsObject): Either[ValidationError, (Evaluation, AccountScriptInfo)] = for {
    evRequest <- UtilsEvaluationRequest.parse(request)
    overriddenBlockchain = evRequest.mkBlockchain(blockchain)
    scriptInfo <- overriddenBlockchain.accountScript(dAppAddress) match {
      case None             => Left(GenericError(s"There is no script on '$dAppAddress'"))
      case Some(scriptInfo) => Right(scriptInfo)
    }
    r <- evRequest match {
      case evRequest: UtilsExprRequest =>
        evRequest.parseCall(scriptInfo.script.stdLibVersion).map { expr =>
          ExprEvaluation(overriddenBlockchain, expr, emptyInvokeScriptLike(dAppAddress))
        }

      case evRequest: UtilsInvocationRequest =>
        evRequest.toInvocation.map { invocation =>
          InvocationEvaluation(overriddenBlockchain, invocation, scriptInfo.script.stdLibVersion, toInvokeScriptLike(invocation, dAppAddress))
        }
    }
  } yield (r, scriptInfo)

  private def emptyInvokeScriptLike(dAppAddress: Address) =
    new InvokeScriptTransactionLike {
      override def dApp: AddressOrAlias              = dAppAddress
      override def funcCall: Terms.FUNCTION_CALL     = Terms.FUNCTION_CALL(FunctionHeader.User(""), Nil)
      override def payments: Seq[Payment]            = Seq.empty
      override def root: InvokeScriptTransactionLike = this
      override val sender: PublicKey                 = PublicKey(ByteStr(new Array[Byte](32)))
      override def assetFee: (Asset, Long)           = Asset.Waves -> FeeConstants(InvokeScript) * ScriptExtraFee
      override def timestamp: Long                   = System.currentTimeMillis()
      override def chainId: Byte                     = AddressScheme.current.chainId
      override def id: Coeval[ByteStr]               = Coeval.evalOnce(ByteStr.empty)
      override val tpe: TransactionType              = TransactionType.InvokeScript
    }

  private def toInvokeScriptLike(invocation: Invocation, dAppAddress: Address) =
    new InvokeScriptTransactionLike {
      override def dApp: AddressOrAlias              = dAppAddress
      override def funcCall: Terms.FUNCTION_CALL     = invocation.funcCall
      override def root: InvokeScriptTransactionLike = this
      override val sender: PublicKey                 = PublicKey(invocation.callerPk)
      override def assetFee: (Asset, Long)           = (Asset.fromCompatId(invocation.feeAssetId), invocation.fee)
      override def timestamp: Long                   = System.currentTimeMillis()
      override def chainId: Byte                     = AddressScheme.current.chainId
      override def id: Coeval[ByteStr]               = Coeval(invocation.transactionId)
      override val tpe: TransactionType              = TransactionType.InvokeScript
      override def payments: Seq[Payment] =
        invocation.payments.payments.map { case (amount, assetId) =>
          Payment(amount, Asset.fromCompatId(assetId))
        }
    }
}

case class ExprEvaluation(blockchain: Blockchain, expr: Terms.EXPR, txLike: InvokeScriptTransactionLike) extends Evaluation {
  def dAppToExpr(dApp: DApp): Either[ValidationError, EXPR] =
    Right(ContractEvaluator.buildSyntheticCall(dApp, expr, ByteStr(DefaultAddress.bytes), DefaultPublicKey))
}

case class InvocationEvaluation(
    blockchain: Blockchain,
    invocation: ContractEvaluator.Invocation,
    stdLibVersion: StdLibVersion,
    txLike: InvokeScriptTransactionLike
) extends Evaluation {
  def dAppToExpr(dApp: DApp): Either[ValidationError, EXPR] =
    ContractEvaluator.buildExprFromInvocation(dApp, invocation, stdLibVersion).bimap(e => GenericError(e.message), _.expr)
}
