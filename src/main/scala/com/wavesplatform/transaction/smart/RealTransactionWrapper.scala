package com.wavesplatform.transaction.smart

import com.wavesplatform.lang.v1.traits.{Proven, _}
import com.wavesplatform.state._
import scodec.bits.ByteVector
import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.transfer._

object RealTransactionWrapper {

  private def header(tx: Transaction): Header = {
    val v = tx match {
      case vt: VersionedTransaction => vt.version
      case _                        => 1
    }
    Header(ByteVector(tx.id().arr), tx.assetFee._2, tx.timestamp, v)
  }
  private def proven(tx: ProvenTransaction): Proven =
    Proven(
      header(tx),
      Recipient.Address(ByteVector(tx.sender.bytes.arr)),
      ByteVector(tx.bodyBytes()),
      ByteVector(tx.sender.publicKey),
      tx.proofs.proofs.map(_.arr).map(ByteVector(_)).toIndexedSeq
    )

  implicit def toByteVector(s: ByteStr): ByteVector = ByteVector(s.arr)

  implicit def assetPair(a: AssetPair): APair = APair(a.amountAsset.map(toByteVector), a.priceAsset.map(toByteVector))
  implicit def ord(o: Order): Ord =
    Ord(
      id = ByteVector(o.id.value.arr),
      sender = Recipient.Address(ByteVector(o.sender.bytes.arr)),
      senderPublicKey = ByteVector(o.senderPublicKey.publicKey),
      matcherPublicKey = ByteVector(o.matcherPublicKey.publicKey),
      assetPair = o.assetPair,
      orderType = o.orderType match {
        case BUY  => OrdType.Buy
        case SELL => OrdType.Sell
      },
      price = o.price,
      amount = o.amount,
      timestamp = o.timestamp,
      expiration = o.expiration,
      matcherFee = o.matcherFee,
      bodyBytes = ByteVector(o.bodyBytes()),
      proofs = IndexedSeq(ByteVector(o.signature))
    )

  implicit def aoaToRecipient(aoa: AddressOrAlias): Recipient = aoa match {
    case a: Address => Recipient.Address(ByteVector(a.bytes.arr))
    case a: Alias   => Recipient.Alias(a.name)
  }

  def apply(tx: Transaction): Tx = {
    tx match {
      case g: GenesisTransaction => Tx.Genesis(header(g), g.amount, g.recipient)
      case t: TransferTransaction =>
        Tx.Transfer(
          proven(t),
          feeAssetId = t.feeAssetId.map(a => ByteVector(a.arr)),
          assetId = t.assetId.map(a => ByteVector(a.arr)),
          amount = t.amount,
          recipient = t.recipient,
          attachment = ByteVector(t.attachment)
        )
      case i: IssueTransaction =>
        Tx.Issue(proven(i),
                 i.quantity,
                 ByteVector(i.name),
                 ByteVector(i.description),
                 i.reissuable,
                 i.decimals,
                 i.script.map(_.bytes()).map(toByteVector))
      case r: ReissueTransaction     => Tx.ReIssue(proven(r), r.quantity, r.assetId, r.reissuable)
      case b: BurnTransaction        => Tx.Burn(proven(b), b.quantity, b.assetId)
      case b: LeaseTransaction       => Tx.Lease(proven(b), b.amount, b.recipient)
      case b: LeaseCancelTransaction => Tx.LeaseCancel(proven(b), b.leaseId)
      case b: CreateAliasTransaction => Tx.CreateAlias(proven(b), b.alias.name)
      case ms: MassTransferTransaction =>
        Tx.MassTransfer(
          proven(ms),
          assetId = ms.assetId.map(a => ByteVector(a.arr)),
          transferCount = ms.transfers.length,
          totalAmount = ms.transfers.map(_.amount).sum,
          transfers = ms.transfers.map(r => com.wavesplatform.lang.v1.traits.TransferItem(r.address, r.amount)).toIndexedSeq,
          attachment = ByteVector(ms.attachment)
        )
      case ss: SetScriptTransaction => Tx.SetScript(proven(ss), ss.script.map(_.bytes()).map(toByteVector))
      case p: PaymentTransaction    => Tx.Payment(proven(p), p.amount, p.recipient)
      case e: ExchangeTransaction   => Tx.Exchange(proven(e), e.price, e.amount, e.buyMatcherFee, e.sellMatcherFee, e.buyOrder, e.sellOrder)
      case s: SponsorFeeTransaction => Tx.Sponsorship(proven(s), s.assetId, s.minSponsoredAssetFee)
      case d: DataTransaction =>
        Tx.Data(
          proven(d),
          d.data.map {
            case IntegerDataEntry(key, value) => DataItem.Lng(key, value)
            case StringDataEntry(key, value)  => DataItem.Str(key, value)
            case BooleanDataEntry(key, value) => DataItem.Bool(key, value)
            case BinaryDataEntry(key, value)  => DataItem.Bin(key, value)
          }.toIndexedSeq
        )
    }
  }

  def block(b: BlockHeader, h: Int): Blk = Blk(b.timestamp, h, b.consensusData.generationSignature)
}
