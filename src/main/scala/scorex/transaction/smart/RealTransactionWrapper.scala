package scorex.transaction.smart

import com.wavesplatform.lang.v1.traits.{Header, Proven, Recipient, Tx}
import com.wavesplatform.state.ByteStr
import scodec.bits.ByteVector
import scorex.account.{Address, AddressOrAlias, Alias}
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.transfer._

object RealTransactionWrapper {

  private def header(tx: Transaction): Header = Header(ByteVector(tx.id().arr), tx.assetFee._2, tx.timestamp, 0)
  private def proven(tx: ProvenTransaction): Proven =
    Proven(header(tx), ByteVector(tx.bodyBytes()), ByteVector(tx.sender.publicKey), tx.proofs.proofs.map(_.arr).map(ByteVector(_)).toIndexedSeq)

  implicit def aoaToRecipient(aoa: AddressOrAlias): Recipient = aoa match {
    case a: Address => Recipient.Address(ByteVector(a.bytes.arr))
    case a: Alias   => Recipient.Alias(a.name)
  }

  implicit def toByteVector(s: ByteStr): ByteVector = ByteVector(s.arr)

  def apply(tx: Transaction): Tx = {
    tx match {
      case g: GenesisTransaction => Tx.Genesis(header(g), g.amount, g.recipient)
      case t: TransferTransaction =>
        Tx.Transfer(
          proven(t),
          feeAssetId = t.feeAssetId.map(a => ByteVector(a.arr)),
          transferAssetId = t.assetId.map(a => ByteVector(a.arr)),
          amount = t.amount,
          recipient = t.recipient,
          attachment = ByteVector(t.attachment)
        )
      case i: IssueTransaction       => Tx.Issue(proven(i), i.quantity, i.assetId(), ByteVector(i.description), i.reissuable)
      case r: ReissueTransaction     => Tx.ReIssue(proven(r), r.quantity, r.reissuable)
      case b: BurnTransaction        => Tx.Burn(proven(b), b.quantity)
      case b: LeaseTransaction       => Tx.Lease(proven(b), b.amount, b.recipient)
      case b: LeaseCancelTransaction => Tx.LeaseCancel(proven(b), b.leaseId)
      case b: CreateAliasTransaction => Tx.CreateAlias(proven(b), b.alias.name)
      case ms: MassTransferTransaction =>
        Tx.MassTransfer(
          proven(ms),
          transferAssetId = ms.assetId.map(a => ByteVector(a.arr)),
          transfers = ms.transfers.map(r => com.wavesplatform.lang.v1.traits.TransferItem(r.address, r.amount)).toIndexedSeq,
          attachment = ByteVector(ms.attachment)
        )
      case ss: SetScriptTransaction => Tx.SetScript(proven(ss), ss.script.map(_.bytes().arr).map(ByteVector(_)))
      case p: PaymentTransaction    => Tx.Payment(proven(p), p.amount, p.recipient)
      case e: ExchangeTransaction   => Tx.Exchange(proven(e))
      case s: SponsorFeeTransaction => Tx.Sponsorship(proven(s), s.minSponsoredAssetFee)
    }
  }
}
