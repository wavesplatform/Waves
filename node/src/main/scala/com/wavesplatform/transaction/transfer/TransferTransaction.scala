package com.wavesplatform.transaction.transfer

import java.nio.ByteBuffer

import cats.syntax.apply._
import cats.syntax.either._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.{AddressOrAlias, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.crypto.{KeyLength, SignatureLength}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import com.wavesplatform.transaction.transfer.TransferTransaction.transactionType
import com.wavesplatform.transaction.validation.{validateAmount, validateAttachment, validateFee}
import com.wavesplatform.utils.base58Length
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.reflect.ClassTag
import scala.util.{Failure, Try}

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
      sender.bytes.arr,
      assetId.byteRepr,
      feeAssetId.byteRepr,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(amount),
      Longs.toByteArray(fee),
      recipient.bytes.arr,
      Deser.serializeArray(attachment)
    )
  }

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(version match {
    case 1 => Array(transactionType.toByte) ++ _bytesBase()
    case 2 => Array(transactionType.toByte, version) ++ _bytesBase()
  })

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(version match {
    case 1 => Bytes.concat(Array(transactionType.toByte), proofs.proofs.head, bodyBytes())
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

  //todo: (NODE-1915) remove after refactoring
  override def builder: TransactionParser = TransferTransaction.transactionParserStub
}

object TransferTransaction extends TransactionParserLite {

  val transactionType: TransactionType = TransactionType(4)
  val typeId: Byte                     = transactionType.toByte

  override def supportedVersions: Set[Byte] = Set(1.toByte, 2.toByte)

  override type TransactionT = TransferTransaction

  override def classTag: ClassTag[TransferTransaction] = ClassTag(classOf[TransferTransaction])

  implicit class ByteBufferOps(val buf: ByteBuffer) extends AnyVal {
    def getPrefixedByteArray: Array[Byte] = {
      val prefix = buf.getShort
      require(prefix >= 0, "negative array length")
      if (prefix > 0) getByteArray(prefix) else Array.emptyByteArray
    }

    def getAsset: Asset = {
      val prefix = buf.get
      if (prefix == 0) Asset.Waves
      else if (prefix == 1) Asset.IssuedAsset(ByteStr(getByteArray(AssetIdLength)))
      else throw new IllegalArgumentException(s"Invalid asset id prefix: $prefix")
    }

    def getAddressOrAlias: AddressOrAlias = ???

    def getByteArray(size: Int): Array[Byte] = {
      val result = new Array[Byte](size)
      buf.get(result)
      result
    }

    def getSignature: Array[Byte] = getByteArray(SignatureLength)

    def getPublicKey: PublicKey = PublicKey(getByteArray(KeyLength))
  }

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

  override def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] = Try {
    require(bytes.length > 2, "buffer underflow while parsing transfer transaction")

    if (bytes(0) == 0) {
      require(bytes(1) == typeId, "transaction type mismatch")
      val buf = ByteBuffer.wrap(bytes, 3, bytes.length - 3)
      val tx  = parseCommonPart(2, buf)
      tx
    } else {
      require(bytes(0) == typeId, "transaction type mismatch")
      val buf       = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
      val signature = buf.getSignature
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
      .map { unsigned =>
        unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
      }

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

  // todo: (NODE-1915) #forTypeSet used in AddressTransactions, rewrite to new parsers
  val transactionParserStub: TransactionParser = new TransactionParserFor[TransferTransaction]() {
    override def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] = TransferTransaction.parseBytes(bytes)

    override def typeId: Byte                                                      = TransferTransaction.typeId
    override def supportedVersions: Set[Byte]                                      = TransferTransaction.supportedVersions.map(_.toByte)
    override protected def parseHeader(bytes: Array[Byte]): Try[Int]               = Failure(new NotImplementedError)
    override protected def parseTail(bytes: Array[Byte]): Try[TransferTransaction] = Failure(new NotImplementedError)
    override val byteHeaderDescription: ByteEntity[Unit]                           = Nop()
    override val byteTailDescription: ByteEntity[TransferTransaction]              = Nop()
  }
}
