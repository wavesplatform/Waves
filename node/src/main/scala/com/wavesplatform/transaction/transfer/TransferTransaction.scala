package com.wavesplatform.transaction.transfer

import java.nio.ByteBuffer

import cats.syntax.apply._
import cats.syntax.either._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.serialization.Deser.{ByteBufferOps, serializeArray}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.validation._
import com.wavesplatform.utils.base58Length
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.reflect.ClassTag
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
) extends VersionedTransaction
    with SignatureField
    with FastHashId {

  override val typeId: Byte = TransferTransaction.typeId

  override val assetFee: (Asset, Long) = (feeAssetId, fee)

  private val _bytesBase: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      sender,
      assetId.byteRepr,
      feeAssetId.byteRepr,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(amount),
      Longs.toByteArray(fee),
      recipient.bytes.arr,
      serializeArray(attachment)
    )
  }

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(version match {
    case 1 => Array(typeId) ++ _bytesBase()
    case 2 => Array(typeId, version) ++ _bytesBase()
  })

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(version match {
    case 1 => Bytes.concat(Array(typeId), proofs.proofs.head, bodyBytes())
    case 2 => Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes())
  })

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

  override def builder: TransactionParserLite = TransferTransaction
}

object TransferTransaction extends TransactionParserLite {

  val typeId: Byte = 4.toByte

  override def supportedVersions: Set[Byte] = Set(1.toByte, 2.toByte)

  override type TransactionT = TransferTransaction

  override def classTag: ClassTag[TransferTransaction] = ClassTag(classOf[TransferTransaction])

  private def parseCommonPart(version: Byte, buf: ByteBuffer): TransferTransaction = {
    val sender     = buf.getPublicKey
    val assetId    = buf.getAsset
    val feeAssetId = buf.getAsset
    val ts         = buf.getLong
    val amount     = buf.getLong
    val fee        = buf.getLong
    val recipient  = buf.getAddressOrAlias
    val attachment = buf.getPrefixedByteArray

    TransferTransaction(version, ts, sender, recipient, assetId, amount, feeAssetId, fee, attachment, Proofs.empty)
  }

  private def parseProofs(buf: ByteBuffer): Proofs =
    Proofs.fromBytes(buf.getByteArray(buf.remaining())).explicitGet()

  override def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] = Try {
    require(bytes.length > 2, "buffer underflow while parsing transfer transaction")

    if (bytes(0) == 0) {
      require(bytes(1) == typeId, "transaction type mismatch")
      val buf    = ByteBuffer.wrap(bytes, 3, bytes.length - 3)
      val tx     = parseCommonPart(2, buf)
      val proofs = parseProofs(buf)
      tx.copy(proofs = proofs)
    } else {
      require(bytes(0) == typeId, "transaction type mismatch")
      val buf       = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
      val signature = buf.getSignature
      require(buf.get == typeId, "transaction type mismatch")
      parseCommonPart(1, buf).copy(proofs = Proofs(Seq(ByteStr(signature))))
    }
  }

  val MaxAttachmentSize            = 140
  val MaxAttachmentStringSize: Int = base58Length(MaxAttachmentSize)

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

  def sign(tx: TransferTransaction, signer: PrivateKey): Either[ValidationError, TransferTransaction] =
    for {
      proofs <- Proofs.create(Seq(ByteStr(crypto.sign(signer, tx.bodyBytes()))))
    } yield tx.copy(proofs = proofs)

  // create
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

  // signed
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
      .flatMap(sign(_, signer))

  // selfSigned
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
