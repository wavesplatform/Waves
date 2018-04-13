package scorex.transaction.smart

import scodec.bits.ByteVector
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

case class RealTransactionWrapper(tx: Transaction) extends com.wavesplatform.lang.v1.traits.Transaction {
  override def bodyBytes: Either[String, ByteVector] = tx match {
    case pt: ProvenTransaction => Right(ByteVector(pt.bodyBytes()))
    case _                     => Left("Transaction is not Proven, doesn't contain bodyBytes")
  }

  override def transactionType: Int = tx.builder.typeId

  override def senderPk: Either[String, ByteVector] = tx match {
    case pt: Authorized => Right(ByteVector(pt.sender.publicKey))
    case _              => Left("Transaction doesn't contain sender public key")
  }

  override def assetId: Either[String, Option[ByteVector]] = tx match {
    case tt: TransferTransaction           => Right(tt.assetId.map(x => ByteVector(x.arr)))
    case vtt: VersionedTransferTransaction => Right(vtt.assetId.map(x => ByteVector(x.arr)))
    case mtt: MassTransferTransaction      => Right(mtt.assetId.map(x => ByteVector(x.arr)))
    case _                                 => Left("Transaction doesn't contain asset id")
  }

  override def recipient: Either[String, ByteVector] = tx match {
    case pt: PaymentTransaction            => Right(ByteVector(pt.recipient.bytes.arr))
    case tt: TransferTransaction           => Right(ByteVector(tt.recipient.bytes.arr))
    case lt: LeaseTransaction              => Right(ByteVector(lt.recipient.bytes.arr))
    case vtt: VersionedTransferTransaction => Right(ByteVector(vtt.recipient.bytes.arr))
    case _                                 => Left("Transaction doesn't contain recipient")
  }

  override def proofs: Either[String, IndexedSeq[ByteVector]] = tx match {
    case pt: ProvenTransaction => Right(pt.proofs.proofs.map(pf => ByteVector(pf.arr)).toIndexedSeq)
    case _                     => Left("Transaction doesn't contain proofs")
  }

  override def id: ByteVector = ByteVector(tx.id().arr)

  override def fee: Long = tx.assetFee._2

  override def amount: Either[String, Long] = tx match {
    case g: GenesisTransaction           => Right(g.amount)
    case g: PaymentTransaction           => Right(g.amount)
    case g: IssueTransaction             => Right(g.quantity)
    case g: ReissueTransaction           => Right(g.quantity)
    case g: BurnTransaction              => Right(g.amount)
    case g: LeaseTransaction             => Right(g.amount)
    case g: TransferTransaction          => Right(g.amount)
    case g: VersionedTransferTransaction => Right(g.amount)
    case g: ExchangeTransaction          => Right(g.amount)
    case _: CreateAliasTransaction       => Left("Transaction doesn't contain amount")
    case _: SetScriptTransaction         => Left("Transaction doesn't contain amount")
    case _: MassTransferTransaction      => Left("Transaction doesn't contain amount")
    case _: LeaseCancelTransaction       => Left("Transaction doesn't contain amount")
    case _: DataTransaction              => Left("Transaction doesn't contain amount")
  }

  override def feeAssetId: Option[ByteVector] =
    tx.assetFee._1.map(aid => ByteVector(aid.arr))

  override def timestamp: Long = tx.timestamp

  override def aliasText: Either[String, String] = tx match {
    case g: CreateAliasTransaction => Right(g.alias.name)
    case _                         => Left("Transaction doesn't contain alias text")
  }

  override def reissuable: Either[String, Boolean] = tx match {
    case g: SmartIssueTransaction => Right(g.reissuable)
    case g: IssueTransaction      => Right(g.reissuable)
    case g: ReissueTransaction    => Right(g.reissuable)
    case _                        => Left("Transaction doesn't contain reissuable")
  }

  override def decimals: Either[String, Byte] = tx match {
    case g: SmartIssueTransaction => Right(g.decimals)
    case g: IssueTransaction      => Right(g.decimals)
    case _                        => Left("Transaction doesn't contain decimals")
  }

  override def assetDescription: Either[String, ByteVector] = tx match {
    case g: SmartIssueTransaction => Right(ByteVector(g.description))
    case g: IssueTransaction      => Right(ByteVector(g.description))
    case _                        => Left("Transaction doesn't contain asset description")
  }

  override def assetName: Either[String, ByteVector] = tx match {
    case g: SmartIssueTransaction => Right(ByteVector(g.name))
    case g: IssueTransaction      => Right(ByteVector(g.name))
    case _                        => Left("Transaction doesn't contain asset name")
  }

  override def attachment: Either[String, ByteVector] = tx match {
    case g: VersionedTransferTransaction => Right(ByteVector(g.attachment))
    case g: MassTransferTransaction      => Right(ByteVector(g.attachment))
    case g: TransferTransaction          => Right(ByteVector(g.attachment))
    case _                               => Left("Transaction doesn't contain attachment")
  }

  override def chainId: Either[String, Byte] = tx match {
    case g: SetScriptTransaction => Right(g.chainId)
    case _                       => Left("Transaction doesn't contain chainId")
  }

  override def version: Either[String, Byte] = tx match {
    case g: VersionedTransferTransaction => Right(g.version)
    case g: MassTransferTransaction      => Right(g.version)
    case g: SetScriptTransaction         => Right(g.version)
    case g: SmartIssueTransaction        => Right(g.version)
    case g: DataTransaction              => Right(g.version)
    case _                               => Left("Transaction doesn't contain version")
  }
}
