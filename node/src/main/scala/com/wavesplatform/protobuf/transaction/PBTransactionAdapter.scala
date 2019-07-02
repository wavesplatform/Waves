package com.wavesplatform.protobuf.transaction

import com.google.common.primitives.Bytes
import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.crypto
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.protobuf.utils.PBImplicitConversions._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.description.{ByteEntity, BytesArrayUndefinedLength}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.annotation.switch
import scala.reflect.ClassTag
import scala.util.Try

class PBTransactionAdapter(val transaction: PBCachedTransaction) extends VanillaTransaction with Signed with Proven {
  private[this] val txBody: PBTransaction = transaction.transaction.getTransaction

  def isLegacy: Boolean = (txBody.version: @switch) match {
    case 1 | 2 => true
    case _     => false
  }

  //noinspection ScalaStyle
  private lazy val vanillaTx: VanillaTransaction = PBTransactions.vanilla(transaction, unsafe = true).explicitGet()

  override def builder: TransactionParser = PBTransactionAdapter

  override def assetFee: (VanillaAssetId, Long) = PBAmounts.toAssetAndAmount(txBody.getFee)

  override def timestamp: Long = txBody.timestamp

  override val signatureValid: Coeval[Boolean] =
    Coeval.evalOnce((txBody.data.isGenesis || txBody.version > 1) || this.verifySignature())

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    if (isLegacy) vanillaTx.bytes()
    else PBTransactionAdapter.toBytes(this)
  )

  override val proofs: Proofs =
    Proofs(transaction.proofs.map(ByteStr(_)))

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    if (isLegacy) vanillaTx.bodyBytes()
    else transaction.bodyBytes
  )

  override val id: Coeval[ByteStr] = Coeval.evalOnce(
    if (isLegacy) vanillaTx.id()
    else FastHashId.create(this.bodyBytes())
  )

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    if (isLegacy) vanillaTx.json()
    else ??? // TODO: PB json format
  )

  override val sender: PublicKey = txBody.senderPublicKey.publicKey

  override val typeId: Byte = txBody.data match {
    case Data.Empty              => 0
    case Data.Genesis(_)         => GenesisTransaction.typeId
    case Data.Payment(_)         => PaymentTransaction.typeId
    case Data.Issue(_)           => IssueTransaction.typeId
    case Data.Transfer(_)        => TransferTransaction.typeId
    case Data.Reissue(_)         => ReissueTransaction.typeId
    case Data.Burn(_)            => BurnTransaction.typeId
    case Data.Exchange(_)        => ExchangeTransaction.typeId
    case Data.Lease(_)           => LeaseTransaction.typeId
    case Data.LeaseCancel(_)     => LeaseCancelTransaction.typeId
    case Data.CreateAlias(_)     => CreateAliasTransaction.typeId
    case Data.MassTransfer(_)    => MassTransferTransaction.typeId
    case Data.DataTransaction(_) => DataTransaction.typeId
    case Data.SetScript(_)       => SetScriptTransaction.typeId
    case Data.SponsorFee(_)      => SponsorFeeTransaction.typeId
    case Data.SetAssetScript(_)  => SetAssetScriptTransaction.typeId
    case Data.InvokeScript(_)    => InvokeScriptTransaction.typeId
  }

  private[this] def verifySignature(): Boolean =
    proofs.nonEmpty && crypto.verify(proofs.head, bodyBytes(), sender)
}

object PBTransactionAdapter extends TransactionParser.OneVersion {
  def apply(tx: PBCachedTransaction): PBTransactionAdapter = new PBTransactionAdapter(tx)
  def apply(tx: VanillaTransaction): PBTransactionAdapter = tx match {
    case a: PBTransactionAdapter => a
    case _                       => new PBTransactionAdapter(PBTransactions.protobuf(tx))
  }

  def unwrap(tx: VanillaTransaction) = tx match {
    case a: PBTransactionAdapter if a.isLegacy => a.vanillaTx
    case _                                     => tx
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
