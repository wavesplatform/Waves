package com.wavesplatform.transaction.smart

import com.wavesplatform.account._
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator
import com.wavesplatform.state.diffs.invoke.{InvokeScriptLike, InvokeScriptTransactionLike}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.serialization.impl.InvokeScriptTxSerializer
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.{DefaultFuncCall, Payment}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.InvokeTransaction.defaultCall
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.InvokeScriptTxValidator
import monix.eval.Coeval
import play.api.libs.json._

import scala.util.Try

case class InvokeScriptTransaction(
    version: TxVersion,
    sender: PublicKey,
    dApp: AddressOrAlias,
    funcCallOpt: Option[FUNCTION_CALL],
    payments: Seq[Payment],
    fee: TxAmount,
    feeAssetId: Asset,
    override val timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends InvokeTransaction
    with PBSince.V2 {

  override val funcCall = funcCallOpt.getOrElse(DefaultFuncCall)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(InvokeScriptTxSerializer.bodyBytes(this))
  val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(InvokeScriptTxSerializer.toBytes(this))
  val json: Coeval[JsObject]         = Coeval.evalOnce(InvokeScriptTxSerializer.toJson(this))

  override val enableEmptyKeys: Boolean              = isProtobufVersion
  override def senderAddress: Address                 = sender.toAddress
}

object InvokeScriptTransaction extends TransactionParser {
  type TransactionT = InvokeScriptTransaction

  override val typeId: TxType                    = 16: Byte
  override val supportedVersions: Set[TxVersion] = Set(1, 2)

  implicit val validator: TxValidator[InvokeScriptTransaction] = InvokeScriptTxValidator

  implicit def sign(tx: InvokeScriptTransaction, privateKey: PrivateKey): InvokeScriptTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  override def parseBytes(bytes: Array[Byte]): Try[InvokeScriptTransaction] =
    InvokeScriptTxSerializer.parseBytes(bytes)

  case class Payment(amount: TxAmount, assetId: Asset)
  object Payment {
    implicit val jsonFormat: Format[Payment] = Json.format
  }

  val DefaultFuncCall: FUNCTION_CALL = FUNCTION_CALL(User(ContractEvaluator.DEFAULT_FUNC_NAME), Nil)

  def create(
      version: TxVersion,
      sender: PublicKey,
      dappAddress: AddressOrAlias,
      fc: Option[FUNCTION_CALL],
      p: Seq[Payment],
      fee: TxAmount,
      feeAssetId: Asset,
      timestamp: TxTimestamp,
      proofs: Proofs,
      chainId: Byte
  ): Either[ValidationError, InvokeScriptTransaction] =
    InvokeScriptTransaction(version, sender, dappAddress, fc, p, fee, feeAssetId, timestamp, proofs, chainId).validatedEither
}
