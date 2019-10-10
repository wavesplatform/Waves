package com.wavesplatform.transaction.transfer

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.{AddressOrAlias, PrivateKey, PublicKey}
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.utils.base58Length
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

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

  override val bodyBytes               = Coeval.evalOnce(TransferTransaction.bodyBytes(this))
  override val bytes                   = Coeval.evalOnce(TransferTransaction.bytes(this))
  override val assetFee: (Asset, Long) = (feeAssetId, fee)

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

  def validate(tx: TransferTransaction): Either[ValidationError, Unit] = ???

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
  ): Either[ValidationError, TransferTransaction] = ???
}
