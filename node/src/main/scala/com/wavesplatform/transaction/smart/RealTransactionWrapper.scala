package com.wavesplatform.transaction.smart

import cats.syntax.either._
import com.wavesplatform.account.{AddressOrAlias, Recipient}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.traits.domain.Tx.{Header, Proven}
import com.wavesplatform.lang.v1.traits.domain.{Recipient => RideRecipient, _}
import com.wavesplatform.protobuf.ByteStringExt
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
      case _                        => TxVersion.V1
    }
    Header(txIdOpt.getOrElse(ByteStr(tx.id().arr)), tx.assetFee._2, tx.timestamp, v)
  }
  private def proven(tx: Transaction with ProvenTransaction, txIdOpt: Option[ByteStr] = None): Proven =
    Proven(
      header(tx, txIdOpt),
      RideRecipient.Address(ByteStr(tx.sender.toAddress.bytes)),
      ByteStr(tx.bodyBytes()),
      tx.sender,
      tx.proofs.proofs.map(_.arr).map(ByteStr(_)).toIndexedSeq
    )

  implicit def assetPair(a: AssetPair): APair = APair(a.amountAsset.compatId, a.priceAsset.compatId)
  implicit def ord(o: Order): Ord =
    Ord(
      id = o.id(),
      sender = RideRecipient.Address(ByteStr(o.sender.toAddress.bytes)),
      senderPublicKey = o.senderPublicKey,
      matcherPublicKey = o.matcherPublicKey,
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

  def apply(
      tx: Transaction,
      blockchain: Blockchain,
      stdLibVersion: StdLibVersion,
      target: AttachedPaymentTarget
  ): Either[ExecutionError, Tx] =
    (tx: @unchecked) match {
      case g: GenesisTransaction  => Tx.Genesis(header(g), g.amount, toRide(g.recipient)).asRight
      case t: TransferTransaction => mapTransferTx(t).asRight
      case i: IssueTransaction =>
        Tx.Issue(
            proven(i),
            i.quantity,
            i.name.toByteStr,
            i.description.toByteStr,
            i.reissuable,
            i.decimals,
            i.script.map(_.bytes())
          )
          .asRight
      case r: ReissueTransaction     => Tx.ReIssue(proven(r), r.quantity, r.asset.id, r.reissuable).asRight
      case b: BurnTransaction        => Tx.Burn(proven(b), b.quantity, b.asset.id).asRight
      case b: LeaseTransaction       => Tx.Lease(proven(b), b.amount, toRide(b.recipient)).asRight
      case b: LeaseCancelTransaction => Tx.LeaseCancel(proven(b), b.leaseId).asRight
      case b: CreateAliasTransaction => Tx.CreateAlias(proven(b), b.alias.name).asRight
      case ms: MassTransferTransaction =>
        Tx.MassTransfer(
            proven(ms),
            assetId = ms.assetId.compatId,
            transferCount = ms.transfers.length,
            totalAmount = ms.transfers.map(_.amount).sum,
            transfers = ms.transfers.map(r => com.wavesplatform.lang.v1.traits.domain.Tx.TransferItem(toRide(r.address), r.amount)).toIndexedSeq,
            attachment = ms.attachment
          )
          .asRight
      case ss: SetScriptTransaction      => Tx.SetScript(proven(ss), ss.script.map(_.bytes())).asRight
      case ss: SetAssetScriptTransaction => Tx.SetAssetScript(proven(ss), ss.asset.id, ss.script.map(_.bytes())).asRight
      case p: PaymentTransaction         => Tx.Payment(proven(p), p.amount, toRide(p.recipient)).asRight
      case e: ExchangeTransaction        => Tx.Exchange(proven(e), e.amount, e.price, e.buyMatcherFee, e.sellMatcherFee, e.buyOrder, e.sellOrder).asRight
      case s: SponsorFeeTransaction      => Tx.Sponsorship(proven(s), s.asset.id, s.minSponsoredAssetFee).asRight
      case d: DataTransaction =>
        Tx.Data(
            proven(d),
            d.data.collect {
              case IntegerDataEntry(key, value) => DataItem.Lng(key, value)
              case StringDataEntry(key, value)  => DataItem.Str(key, value)
              case BooleanDataEntry(key, value) => DataItem.Bool(key, value)
              case BinaryDataEntry(key, value)  => DataItem.Bin(key, value)
              case EmptyDataEntry(key)          => DataItem.Delete(key)
            }.toIndexedSeq
          )
          .asRight
      case ci: InvokeScriptTransaction =>
        AttachedPaymentExtractor
          .extractPayments(ci, stdLibVersion, blockchain, target)
          .map { payments =>
            Tx.CI(
              proven(ci),
              toRide(ci.dApp),
              payments,
              ci.feeAssetId.compatId,
              ci.funcCallOpt.map(_.function.funcName),
              ci.funcCallOpt.map(_.args.map(arg => arg.asInstanceOf[EVALUATED])).getOrElse(List.empty)
            )
          }

      case u: UpdateAssetInfoTransaction =>
        Tx.UpdateAssetInfo(proven(u), u.assetId.id, u.name, u.description).asRight
    }

  def mapTransferTx(t: TransferTransaction): Tx.Transfer =
    Tx.Transfer(
      proven(t),
      feeAssetId = t.feeAssetId.compatId,
      assetId = t.assetId.compatId,
      amount = t.amount,
      recipient = toRide(t.recipient),
      attachment = t.attachment
    )

  def toRide(recipient: Recipient): RideRecipient = ???
  def toRide(aoa: AddressOrAlias): RideRecipient  = ???
}
