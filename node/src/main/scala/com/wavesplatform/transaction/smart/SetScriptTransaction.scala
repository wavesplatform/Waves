package com.wavesplatform.transaction.smart

import com.google.common.primitives.Bytes
import com.wavesplatform.account._
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.serialization.impl.SetScriptTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.TxFeeValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

case class SetScriptTransaction(
    version: TxVersion,
    sender: PublicKey,
    script: Option[Script],
    fee: TxAmount,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: ChainId
) extends ProvenTransaction
    with VersionedTransaction
    with TxWithFee.InWaves
    with FastHashId
    with LegacyPBSwitch.V2 {

  //noinspection TypeAnnotation
  override val builder = SetScriptTransaction

  val bodyBytes: Coeval[Array[Byte]]      = Coeval.evalOnce(builder.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))
  override val json: Coeval[JsObject]     = Coeval.evalOnce(builder.serializer.toJson(this))
}

object SetScriptTransaction extends TransactionParser {
  override val typeId: TxType                    = 13
  override val supportedVersions: Set[TxVersion] = Set(1, 2)

  implicit val validator: TxValidator[SetScriptTransaction] =
    TxFeeValidator.asInstanceOf[TxValidator[SetScriptTransaction]]

  implicit def sign(tx: SetScriptTransaction, privateKey: PrivateKey): SetScriptTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  val serializer = SetScriptTxSerializer

  override def parseBytes(bytes: Array[TxVersion]): Try[SetScriptTransaction] =
    serializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      script: Option[Script],
      fee: TxAmount,
      timestamp: TxTimestamp,
      proofs: Proofs
  ): Either[ValidationError, SetScriptTransaction] =
    SetScriptTransaction(version, sender, script, fee, timestamp, proofs, ChainId.current).validatedEither

  def signed(
      version: TxVersion,
      sender: PublicKey,
      script: Option[Script],
      fee: TxAmount,
      timestamp: TxTimestamp,
      signer: PrivateKey
  ): Either[ValidationError, SetScriptTransaction] =
    create(version, sender, script, fee, timestamp, Proofs.empty).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      script: Option[Script],
      fee: TxAmount,
      timestamp: TxTimestamp
  ): Either[ValidationError, SetScriptTransaction] =
    signed(version, sender, script, fee, timestamp, sender)
}
