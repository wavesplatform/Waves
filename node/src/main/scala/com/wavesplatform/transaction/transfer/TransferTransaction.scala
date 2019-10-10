package com.wavesplatform.transaction.transfer

import cats.syntax.apply._
import cats.syntax.either._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.{AddressOrAlias, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.validation.{validateAmount, validateAttachment, validateFee}
import com.wavesplatform.utils.base58Length
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

case class TransferTransaction(
    version: Byte,
    timestamp: Long,
    sender: PublicKey,
    recipient: AddressOrAlias,
    assetId: Asset,
    amount: Long,
    feeAssetId: Asset,
    fee: Long,
    attachment: Array[Byte],
    proofs: Proofs
) extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val typeId: Byte                   = TransferTransaction.typeId
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(TransferTransaction.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(TransferTransaction.bytes(this))
  override val assetFee: (Asset, Long)        = (feeAssetId, fee)

  override final val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"    -> version,
      "recipient"  -> recipient.stringRepr,
      "assetId"    -> assetId.maybeBase58Repr,
      "feeAsset"   -> feeAssetId.maybeBase58Repr, // legacy v0.11.1 compat
      "amount"     -> amount,
      "attachment" -> Base58.encode(attachment)
    )
  )

  override def checkedAssets(): Seq[IssuedAsset] = assetId match {
    case Waves          => Seq()
    case a: IssuedAsset => Seq(a)
  }

  //TODO: remove after refactoring
  override def builder: TransactionParser = ???
}

object TransferTransaction {

  val typeId: Byte = 4

  val MaxAttachmentSize            = 140
  val MaxAttachmentStringSize: Int = base58Length(MaxAttachmentSize)

  final protected def bytesBase(t: TransferTransaction): Array[Byte] = {
    val timestampBytes  = Longs.toByteArray(t.timestamp)
    val assetIdBytes    = t.assetId.byteRepr
    val feeAssetIdBytes = t.feeAssetId.byteRepr
    val amountBytes     = Longs.toByteArray(t.amount)
    val feeBytes        = Longs.toByteArray(t.fee)

    Bytes.concat(
      t.sender,
      assetIdBytes,
      feeAssetIdBytes,
      timestampBytes,
      amountBytes,
      feeBytes,
      t.recipient.bytes.arr,
      Deser.serializeArray(t.attachment)
    )
  }

  def bodyBytes(t: TransferTransaction): Array[Byte] = t.version match {
    case 1 => Array(typeId) ++ bytesBase(t)
    case 2 => Array(typeId, t.version) ++ bytesBase(t)
  }

  def bytes(t: TransferTransaction): Array[Byte] = t.version match {
    case 1 => Bytes.concat(Array(typeId), t.proofs.proofs.head, bodyBytes(t))
    case 2 => Bytes.concat(Array(0: Byte), bodyBytes(t), t.proofs.bytes())
  }

  def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] = ???

  def validate(tx: TransferTransaction): Either[ValidationError, Unit] =
    validate(tx.amount, tx.assetId, tx.fee, tx.feeAssetId, tx.attachment)

  //noinspection UnnecessaryPartialFunction
  def validate(amt: Long, maybeAmtAsset: Asset, feeAmt: Long, maybeFeeAsset: Asset, attachment: Array[Byte]): Either[ValidationError, Unit] =
    (
      validateAmount(amt, maybeAmtAsset.maybeBase58Repr.getOrElse("waves")),
      validateFee(feeAmt),
      validateAttachment(attachment)
    ).mapN { case _ => () }
      .toEither
      .leftMap(_.head)

  def apply(
      version: Byte,
      asset: Asset,
      sender: PublicKey,
      recipient: AddressOrAlias,
      amount: Long,
      timestamp: Long,
      feeAsset: Asset,
      fee: Long,
      attachment: Array[Byte],
      proofs: Proofs
  ): Either[ValidationError, TransferTransaction] =
    validate(amount, asset, fee, feeAsset, attachment)
      .map(_ => TransferTransaction(version, timestamp, sender, recipient, asset, amount, feeAsset, fee, attachment, proofs))

  def apply(
      version: Byte,
      asset: Asset,
      sender: PublicKey,
      recipient: AddressOrAlias,
      amount: Long,
      timestamp: Long,
      feeAsset: Asset,
      fee: Long,
      attachment: Array[Byte],
      signer: PrivateKey
  ): Either[ValidationError, TransferTransaction] =
    apply(version, asset, sender, recipient, amount, timestamp, feeAsset, fee, attachment, Proofs.empty)
      .map { unsigned =>
        unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
      }

  def selfSigned(
      version: Byte,
      asset: Asset,
      sender: KeyPair,
      recipient: AddressOrAlias,
      amount: Long,
      timestamp: Long,
      feeAsset: Asset,
      fee: Long,
      attachment: Array[Byte]
  ): Either[ValidationError, TransferTransaction] =
    apply(version, asset, sender, recipient, amount, timestamp, feeAsset, fee, attachment, sender)
}
