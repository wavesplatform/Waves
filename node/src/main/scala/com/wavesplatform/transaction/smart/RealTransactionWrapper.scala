package com.wavesplatform.transaction.smart

import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.traits.domain.Tx.{Header, Proven}
import com.wavesplatform.lang.v1.traits.domain._
import com.wavesplatform.state._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.transfer._

object RealTransactionWrapper {

  private def header(tx: Transaction, txIdOpt: Option[ByteStr] = None): Header = {
    val v = tx match {
      case vt: VersionedTransaction => vt.version
      case _                        => 1
    }
    Header(txIdOpt.getOrElse(ByteStr(tx.id().arr)), tx.assetFee._2, tx.timestamp, v)
  }
  private def proven(tx: ProvenTransaction, txIdOpt: Option[ByteStr] = None): Proven =
    Proven(
      header(tx, txIdOpt),
      Recipient.Address(ByteStr(tx.sender.bytes.arr)),
      ByteStr(tx.bodyBytes()),
      ByteStr(tx.sender),
      tx.proofs.proofs.map(_.arr).map(ByteStr(_)).toIndexedSeq
    )

  implicit def assetPair(a: AssetPair): APair = APair(a.amountAsset.compatId, a.priceAsset.compatId)
  implicit def ord(o: Order): Ord =
    Ord(
      id = ByteStr(o.id.value.arr),
      sender = Recipient.Address(ByteStr(o.sender.bytes.arr)),
      senderPublicKey = ByteStr(o.senderPublicKey),
      matcherPublicKey = ByteStr(o.matcherPublicKey),
      assetPair = o.assetPair,
      orderType = o.orderType match {
        case BUY  => OrdType.Buy
        case SELL => OrdType.Sell
      },
      amount = o.amount,
      price = o.price,
      timestamp = o.timestamp,
      expiration = o.expiration,
      matcherFee = o.matcherFee,
      bodyBytes = ByteStr(o.bodyBytes()),
      proofs = o.proofs.proofs.map(a => ByteStr(a.arr)).toIndexedSeq,
      matcherFeeAssetId = o.matcherFeeAssetId.compatId
    )

  implicit def aoaToRecipient(aoa: AddressOrAlias): Recipient = aoa match {
    case a: Address => Recipient.Address(ByteStr(a.bytes.arr))
    case a: Alias   => Recipient.Alias(a.name)
  }

  def apply(tx: Transaction, txIdOpt: Option[ByteStr] = None): Tx = {
    tx match {
      case g: GenesisTransaction  => Tx.Genesis(header(g), g.amount, g.recipient)
      case t: TransferTransaction => mapTransferTx(t)
      case i: IssueTransaction =>
        Tx.Issue(proven(i), i.quantity, ByteStr(i.name), ByteStr(i.description), i.reissuable, i.decimals, i.script.map(_.bytes()))
      case r: ReissueTransaction     => Tx.ReIssue(proven(r), r.quantity, r.asset.id, r.reissuable)
      case b: BurnTransaction        => Tx.Burn(proven(b), b.quantity, b.asset.id)
      case b: LeaseTransaction       => Tx.Lease(proven(b), b.amount, b.recipient)
      case b: LeaseCancelTransaction => Tx.LeaseCancel(proven(b), b.leaseId)
      case b: CreateAliasTransaction => Tx.CreateAlias(proven(b), b.alias.name)
      case ms: MassTransferTransaction =>
        Tx.MassTransfer(
          proven(ms),
          assetId = ms.assetId.compatId,
          transferCount = ms.transfers.length,
          totalAmount = ms.transfers.map(_.amount).sum,
          transfers = ms.transfers.map(r => com.wavesplatform.lang.v1.traits.domain.Tx.TransferItem(r.address, r.amount)).toIndexedSeq,
          attachment = ByteStr(ms.attachment)
        )
      case ss: SetScriptTransaction      => Tx.SetScript(proven(ss), ss.script.map(_.bytes()))
      case ss: SetAssetScriptTransaction => Tx.SetAssetScript(proven(ss), ss.asset.id, ss.script.map(_.bytes()))
      case p: PaymentTransaction         => Tx.Payment(proven(p), p.amount, p.recipient)
      case e: ExchangeTransaction        => Tx.Exchange(proven(e), e.amount, e.price, e.buyMatcherFee, e.sellMatcherFee, e.buyOrder, e.sellOrder)
      case s: SponsorFeeTransaction      => Tx.Sponsorship(proven(s), s.asset.id, s.minSponsoredAssetFee)
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
      case ci: InvokeScriptTransaction =>
        Tx.CI(
          proven(ci),
          ci.dAppAddressOrAlias,
          ci.payment.headOption.map(p => Tx.Pmt(p.assetId.compatId, p.amount)),
          ci.feeAssetId.compatId,
          ci.funcCallOpt.map(_.function.funcName),
          ci.funcCallOpt.map(_.args.map(arg => arg.asInstanceOf[EVALUATED])).getOrElse(List.empty)
        )
    }
  }

  def mapTransferTx(t: TransferTransaction): Tx.Transfer =
    Tx.Transfer(
      proven(t),
      feeAssetId = t.feeAssetId.compatId,
      assetId = t.assetId.compatId,
      amount = t.amount,
      recipient = t.recipient,
      attachment = ByteStr(t.attachment)
    )
}
