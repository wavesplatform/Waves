package com.wavesplatform.protobuf.transaction

import com.google.common.primitives.Bytes
import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.protobuf.utils.PBImplicitConversions._
import com.wavesplatform.transaction.description.{ByteEntity, BytesArrayUndefinedLength}
import com.wavesplatform.transaction.{FastHashId, Proofs, Signed, TransactionParser}
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.annotation.switch
import scala.reflect.ClassTag
import scala.util.Try

class PBTransactionAdapter(val transaction: PBCachedTransaction) extends VanillaTransaction with Signed with FastHashId {
  private[this] val txBody: PBTransaction = transaction.transaction.getTransaction

  def isLegacy: Boolean = (txBody.version: @switch) match {
    case 1 | 2 => true
    case _     => false
  }

  //noinspection ScalaStyle
  private[this] lazy val vanillaTx: VanillaTransaction = if (isLegacy) PBTransactions.vanilla(txBody, unsafe = true).explicitGet() else null

  override def builder: TransactionParser = PBTransactionAdapter

  override def assetFee: (VanillaAssetId, Long) = PBAmounts.toAssetAndAmount(txBody.getFee)

  override def timestamp: Long = txBody.timestamp

  override protected val signatureValid: Coeval[Boolean] = Coeval.evalOnce(
    if (isLegacy) PBTransactions.vanilla(txBody).explicitGet() match {
      case s: Signed => s.signatureValid()
      case _         => true
    } else true
  )

  override val bytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(PBTransactionAdapter.toBytes(this))

  override val proofs: Proofs =
    Proofs(transaction.proofs.map(ByteStr(_)))

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    if (isLegacy) vanillaTx.bodyBytes()
    else transaction.bodyBytes
  )

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    if (isLegacy) vanillaTx.json()
    else ??? // TODO: PB json format
  )

  override val sender: PublicKey = txBody.senderPublicKey.publicKey
}

object PBTransactionAdapter extends TransactionParser.OneVersion {
  def apply(tx: PBCachedTransaction): PBTransactionAdapter = new PBTransactionAdapter(tx)
  def apply(tx: VanillaTransaction): PBTransactionAdapter = tx match {
    case a: PBTransactionAdapter => a
    case _                       => new PBTransactionAdapter(PBTransactions.protobuf(tx))
  }

  override def version: Byte = 1

  override type TransactionT = PBTransactionAdapter

  override def classTag: ClassTag[TransactionT] = ClassTag(classOf[PBTransactionAdapter])

  override def typeId: Byte = 0xff.toByte

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] =
    byteTailDescription.deserializeFromByteArray(bytes)

  override val byteTailDescription: ByteEntity[TransactionT] =
    BytesArrayUndefinedLength(0, "PB bytes", Int.MaxValue)
      .map(bs => apply(PBCachedTransaction.fromBytes(bs)))

  private def toBytes(tx: TransactionT): Array[Byte] =
    Bytes.concat(Array[Byte](PBTransactionAdapter.typeId, PBTransactionAdapter.version), tx.transaction.bytes)
}
