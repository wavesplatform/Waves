package com.wavesplatform.transaction.smart

import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.serialization.impl.SetScriptTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.SetScriptTxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

case class SetScriptTransaction(
    version: TxVersion,
    sender: PublicKey,
    script: Option[Script],
    fee: TxPositiveAmount,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(TransactionType.SetScript)
    with ProvenTransaction
    with Versioned.ToV2
    with TxWithFee.InWaves
    with FastHashId
    with PBSince.V2 {
  override type T = SetScriptTransaction
  override def addProof(proof: ByteStr): SetScriptTransaction = copy(proofs = this.proofs.add(proof))

  val bodyBytes: Coeval[Array[Byte]]      = Coeval.evalOnce(SetScriptTxSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(SetScriptTxSerializer.toBytes(this))
  override val json: Coeval[JsObject]     = Coeval.evalOnce(SetScriptTxSerializer.toJson(this))
}

object SetScriptTransaction extends TransactionParser {
  type TransactionT = SetScriptTransaction

  override val typeId: TxType = 13: Byte

  implicit val validator: TxValidator[SetScriptTransaction] = SetScriptTxValidator

  override def parseBytes(bytes: Array[TxVersion]): Try[SetScriptTransaction] =
    SetScriptTxSerializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      script: Option[Script],
      fee: Long,
      timestamp: TxTimestamp,
      proofs: Proofs,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, SetScriptTransaction] =
    for {
      fee <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      tx  <- SetScriptTransaction(version, sender, script, fee, timestamp, proofs, chainId).validatedEither
    } yield tx
}
