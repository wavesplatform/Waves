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
import com.wavesplatform.transaction.description.{
  AddressOrAliasBytes,
  AssetIdBytes,
  ByteEntity,
  BytesArrayUndefinedLength,
  ConstantByte,
  LongBytes,
  OptionBytes,
  ProofsBytes,
  PublicKeyBytes,
  SignatureBytes
}
import com.wavesplatform.transaction.validation.{validateAmount, validateAttachment, validateFee}
import com.wavesplatform.utils.base58Length
import monix.eval.Coeval
import play.api.libs.json.{JsObject, JsString, JsValue, Json}

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
    with FastHashId {

  override val typeId: Byte                   = TransferTransaction.transactionType.toByte
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

  //todo: (NODE-1915) from SignedTransaction
  override def proofField: Seq[(String, JsValue)] =
    super.proofField ++ (if (version == 1.toByte) Seq("signature" -> JsString(this.proofs.head.toString)) else Seq())

  //todo: (NODE-1915) remove after refactoring
  override def builder: TransactionParser = ???
}

object TransferTransaction extends FallbackVersionParser[TransferTransaction] {

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

  override def supportedVersions: Set[TransactionVersion] = Set(TransactionVersion(1), TransactionVersion(2))

  private val V1Body: ByteEntity[TransferTransaction] =
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
    }

  private val V2Body: ByteEntity[TransferTransaction] =
    (
      PublicKeyBytes(1, "Sender's public key"),
      OptionBytes(2, "Asset ID", AssetIdBytes(2, "Asset ID"), "flag (1 - asset, 0 - Waves)"),
      OptionBytes(3, "Fee's asset ID", AssetIdBytes(3, "Fee's asset ID"), "flag (1 - asset, 0 - Waves)"),
      LongBytes(4, "Timestamp"),
      LongBytes(5, "Amount"),
      LongBytes(6, "Fee"),
      AddressOrAliasBytes(7, "Recipient"),
      BytesArrayUndefinedLength(8, "Attachment", TransferTransaction.MaxAttachmentSize),
      ProofsBytes(9)
    ) mapN {
      case (senderPublicKey, assetId, feeAssetId, timestamp, amount, fee, recipient, attachments, proofs) =>
        TransferTransaction(
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
    }

  override def parseBody(body: Array[Byte], version: TransactionVersion): Try[TransferTransaction] =
    if (version == TransactionVersion(1)) V1Body.deserializeFromByteArray(body).flatMap { tx =>
      validate(tx)
        .map(_ => tx)
        .foldToTry
    } else if (version == TransactionVersion(2)) V2Body.deserializeFromByteArray(body).flatMap { tx =>
      validate(tx)
        .map(_ => tx)
        .foldToTry
    } else Failure(new IllegalArgumentException(s"Unknown version: $version"))
}
