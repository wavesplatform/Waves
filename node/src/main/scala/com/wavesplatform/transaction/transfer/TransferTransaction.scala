package com.wavesplatform.transaction.transfer

import cats.syntax.apply._
import cats.syntax.either._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.{AddressOrAlias, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import com.wavesplatform.transaction.validation.{validateAmount, validateAttachment, validateFee}
import com.wavesplatform.utils.base58Length
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

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

  //todo: (NODE-1915) remove after refactoring
  override def builder: TransactionParser = TransferTransaction.transactionParserStub
}

object TransferTransaction extends FallbackVersionParser[TransferTransaction] {
  import ByteReaders._
  import cats.instances.try_._

  val transactionType: TransactionType = TransactionType(4)
  val typeId: Byte                     = transactionType.toByte

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
    case 1 => Array(transactionType.toByte) ++ bytesBase(t)
    case 2 => Array(transactionType.toByte, t.version) ++ bytesBase(t)
  }

  def bytes(t: TransferTransaction): Array[Byte] = t.version match {
    case 1 => Bytes.concat(Array(transactionType.toByte), t.proofs.proofs.head, bodyBytes(t))
    case 2 => Bytes.concat(Array(0: Byte), bodyBytes(t), t.proofs.bytes())
  }

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

  override def supportedVersions: Set[TransactionVersion] = Versions.keySet

  private val Versions: Map[TransactionVersion, Array[Byte] => Try[TransferTransaction]] = Map(
    TransactionVersion(1) -> { body =>
      (
        SignatureBytes(1, "Signature"),
        ConstantByte(2, value = typeId, name = "Transaction type"),
        PublicKeyBytes(3, "Sender's public key"),
        OptionBytes[IssuedAsset](4, "Asset ID", AssetIdBytes(4, "Asset ID"), "flag (1 - asset, 0 - Waves)"),
        OptionBytes[IssuedAsset](5, "Fee's asset ID", AssetIdBytes(5, "Fee's asset ID"), "flag (1 - asset, 0 - Waves)"),
        LongBytes(6, "Timestamp"),
        LongBytes(7, "Amount"),
        LongBytes(8, "Fee"),
        AddressOrAliasBytes(9, "Recipient"),
        BytesArrayUndefinedLength(10, "Attachment", TransferTransaction.MaxAttachmentSize)
      ) mapN {
        case (signature, txId, senderPublicKey, assetId, feeAssetId, timestamp, amount, fee, recipient, attachments) =>
          require(txId == typeId, s"Signed tx id is not match")
          TransferTransaction(
            1.toByte,
            timestamp,
            senderPublicKey,
            recipient,
            assetId.getOrElse(Waves),
            amount,
            feeAssetId.getOrElse(Waves),
            fee,
            attachments,
            Proofs(Seq(signature))
          )
      } deserializeFromByteArray body
    },
    TransactionVersion(2) -> { body =>
      (
        for {
          senderPublicKey <- publicKeyReader
          assetId         <- optionReader(assetIdReader)
          feeAssetId      <- optionReader(assetIdReader)
          timestamp       <- longReader
          amount          <- longReader
          fee             <- longReader
          recipient       <- addressOrAliasReader
          attachments     <- bytesArrayUndefinedLengthReader(TransferTransaction.MaxAttachmentSize)
          proofs          <- proofsReader()
        } yield TransferTransaction(
          2.toByte,
          timestamp,
          senderPublicKey,
          recipient,
          assetId.getOrElse(Waves),
          amount,
          feeAssetId.getOrElse(Waves),
          fee,
          attachments,
          proofs
        )
      ).read(body)
    }
  )

  override def parseBody(body: Array[Byte], version: TransactionVersion): Try[TransferTransaction] =
    Versions.get(version) match {
      case Some(reader) =>
        reader(body).flatMap { tx =>
          validate(tx)
            .map(_ => tx)
            .foldToTry
        }
      case None => Failure(new IllegalArgumentException(s"Unknown version: $version"))
    }

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
