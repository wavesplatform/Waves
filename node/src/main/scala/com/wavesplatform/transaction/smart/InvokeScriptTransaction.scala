package com.wavesplatform.transaction.smart

import com.wavesplatform.account.*
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.state.diffs.invoke.{InvokeScriptLike, InvokeScriptTransactionLike}
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.serialization.impl.InvokeScriptTxSerializer
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.InvokeScriptTxValidator
import monix.eval.Coeval
import play.api.libs.json.*

import scala.util.Try

case class InvokeScriptTransaction(
    version: TxVersion,
    sender: PublicKey,
    dApp: AddressOrAlias,
    funcCallOpt: Option[FUNCTION_CALL],
    payments: Seq[Payment],
    fee: TxPositiveAmount,
    feeAssetId: Asset,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(TransactionType.InvokeScript, payments.collect(InvokeScriptLike.IssuedAssets))
    with InvokeTransaction
    with VersionedTransaction.ToV2
    with PBSince.V2 {

  override def root: InvokeScriptTransactionLike = this
  override val funcCall: FUNCTION_CALL           = funcCallOpt.getOrElse(InvokeTransaction.DefaultCall)
  def senderAddress: Address                     = sender.toAddress

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(InvokeScriptTxSerializer.bodyBytes(this))
  val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(InvokeScriptTxSerializer.toBytes(this))
  val json: Coeval[JsObject]         = Coeval.evalOnce(InvokeScriptTxSerializer.toJson(this))
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

  case class Payment(amount: Long, assetId: Asset)
  object Payment {
    implicit val jsonFormat: Format[Payment] = Json.format
  }

  def create(
      version: TxVersion,
      sender: PublicKey,
      dappAddress: AddressOrAlias,
      fc: Option[FUNCTION_CALL],
      p: Seq[Payment],
      fee: Long,
      feeAssetId: Asset,
      timestamp: TxTimestamp,
      proofs: Proofs,
      chainId: Byte
  ): Either[ValidationError, InvokeScriptTransaction] =
    for {
      fee <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      tx  <- InvokeScriptTransaction(version, sender, dappAddress, fc, p, fee, feeAssetId, timestamp, proofs, chainId).validatedEither
    } yield tx
}
