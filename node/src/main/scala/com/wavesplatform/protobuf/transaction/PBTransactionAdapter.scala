package com.wavesplatform.protobuf.transaction

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.utils._
import com.wavesplatform.protobuf.utils.PBImplicitConversions._
import com.wavesplatform.transaction.description.{ByteEntity, BytesArrayUndefinedLength}
import com.wavesplatform.transaction.{FastHashId, Proofs, Signed, TransactionParser}
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.annotation.switch
import scala.reflect.ClassTag
import scala.util.Try

class PBTransactionAdapter(tx: PBCachedTransaction) extends VanillaTransaction with Signed with FastHashId {
  def isLegacy: Boolean = (tx.transaction.getTransaction.version: @switch) match {
    case 1 | 2 => true
    case _     => false
  }

  //noinspection ScalaStyle
  private[this] lazy val vanillaTx: VanillaTransaction = if (isLegacy) PBTransactions.vanilla(tx, unsafe = true).explicitGet() else null

  override def builder: TransactionParser = PBTransactionAdapter

  override def assetFee: (VanillaAssetId, Long) = PBAmounts.toAssetAndAmount(tx.transaction.getTransaction.getFee)

  override def timestamp: Long = tx.transaction.getTransaction.timestamp

  override protected val signatureValid: Coeval[Boolean] = Coeval.evalOnce(
    if (isLegacy) PBTransactions.vanilla(tx).explicitGet() match {
      case s: Signed => s.signatureValid()
      case _         => true
    } else true
  )

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    if (isLegacy) vanillaTx.bytes()
    else tx.bytes
  )

  override def proofs: Proofs = ???

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    if (isLegacy) vanillaTx.bodyBytes()
    else tx.bodyBytes
  )
  override val json: Coeval[JsObject] = Coeval.evalOnce(
    if (isLegacy) vanillaTx.json()
    else ???
  )

  override val sender: PublicKey = tx.transaction.getTransaction.senderPublicKey.publicKey
}

object PBTransactionAdapter extends TransactionParser.OneVersion {
  def apply(tx: PBCachedTransaction): PBTransactionAdapter = new PBTransactionAdapter(tx)

  override def version: Byte = 1

  override type TransactionT = PBTransactionAdapter

  override def classTag: ClassTag[TransactionT] = ClassTag(classOf[PBTransactionAdapter])

  override def typeId: Byte = 0xff.toByte

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] =
    byteTailDescription.deserializeFromByteArray(bytes)

  override val byteTailDescription: ByteEntity[TransactionT] =
    BytesArrayUndefinedLength(0, "PB bytes", Int.MaxValue)
      .map(bs => new PBTransactionAdapter(PBCachedTransaction.fromBytes(bs)))
}
