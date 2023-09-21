package com.wavesplatform.transaction.smart

import cats.syntax.either.*
import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.traits.domain.Tx.{Header, Proven}
import com.wavesplatform.lang.v1.traits.domain.{Recipient as RideRecipient, *}
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionLike
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.{EthereumTransaction, *}

object RealTransactionWrapper {
  private def header(tx: Transaction, txIdOpt: Option[ByteStr] = None): Header = {
    val v = tx match {
      case _: EthereumTransaction   => 0.toByte
      case vt: VersionedTransaction => vt.version
      case _                        => TxVersion.V1
    }
    Header(txIdOpt.getOrElse(ByteStr(tx.id().arr)), tx.fee, tx.timestamp, v)
  }

  private def proven(tx: AuthorizedTransaction, txIdOpt: Option[ByteStr] = None, emptyBodyBytes: Boolean = false): Proven = {
    val proofs = tx match {
      case p: ProvenTransaction => p.proofs.map(_.arr).map(ByteStr(_)).toIndexedSeq
      case _                    => Vector()
    }
    Proven(
      header(tx, txIdOpt),
      RideRecipient.Address(ByteStr(tx.sender.toAddress.bytes)),
      if (emptyBodyBytes) ByteStr.empty else ByteStr(tx.bodyBytes()),
      tx.sender,
      proofs
    )
  }

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
      amount = o.amount.value,
      price = o.price.value,
      timestamp = o.timestamp,
      expiration = o.expiration,
      matcherFee = o.matcherFee.value,
      bodyBytes = ByteStr(o.bodyBytes()),
      proofs = o.proofs.proofs.map(a => ByteStr(a.arr)).toIndexedSeq,
      matcherFeeAssetId = o.matcherFeeAssetId.compatId,
      attachment = o.attachment
    )

  def apply(
      tx: TransactionBase,
      blockchain: Blockchain,
      stdLibVersion: StdLibVersion,
      target: AttachedPaymentTarget
  ): Either[String, Tx] =
    (tx: @unchecked) match {
      case g: GenesisTransaction      => Tx.Genesis(header(g), g.amount.value, toRide(g.recipient)).asRight
      case t: TransferTransactionLike => mapTransferTx(t).asRight
      case i: IssueTransaction =>
        Tx.Issue(
          proven(i),
          i.quantity.value,
          i.name.toByteStr,
          i.description.toByteStr,
          i.reissuable,
          i.decimals.value,
          i.script.map(_.bytes())
        ).asRight
      case r: ReissueTransaction     => Tx.ReIssue(proven(r), r.quantity.value, r.asset.id, r.reissuable).asRight
      case b: BurnTransaction        => Tx.Burn(proven(b), b.quantity.value, b.asset.id).asRight
      case b: LeaseTransaction       => Tx.Lease(proven(b), b.amount.value, toRide(b.recipient)).asRight
      case b: LeaseCancelTransaction => Tx.LeaseCancel(proven(b), b.leaseId).asRight
      case b: CreateAliasTransaction => Tx.CreateAlias(proven(b), b.alias.name).asRight
      case ms: MassTransferTransaction =>
        Tx.MassTransfer(
          proven(ms),
          assetId = ms.assetId.compatId,
          transferCount = ms.transfers.length,
          totalAmount = ms.transfers.map(_.amount.value).sum,
          transfers = ms.transfers.map(r => com.wavesplatform.lang.v1.traits.domain.Tx.TransferItem(toRide(r.address), r.amount.value)).toIndexedSeq,
          attachment = ms.attachment
        ).asRight
      case ss: SetScriptTransaction      => Tx.SetScript(proven(ss), ss.script.map(_.bytes())).asRight
      case ss: SetAssetScriptTransaction => Tx.SetAssetScript(proven(ss), ss.asset.id, ss.script.map(_.bytes())).asRight
      case p: PaymentTransaction         => Tx.Payment(proven(p), p.amount.value, toRide(p.recipient)).asRight
      case e: ExchangeTransaction =>
        Tx.Exchange(proven(e), e.amount.value, e.price.value, e.buyMatcherFee, e.sellMatcherFee, e.buyOrder, e.sellOrder).asRight
      case s: SponsorFeeTransaction => Tx.Sponsorship(proven(s), s.asset.id, s.minSponsoredAssetFee.map(_.value)).asRight
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
        ).asRight

      case ie: InvokeExpressionTransaction =>
        Tx.InvokeExpression(proven(ie), ie.expressionBytes, ie.feeAssetId.compatId).asRight

      case ci: InvokeScriptTransactionLike =>
        val (version, bodyBytes, proofs) = ci match {
          case ist: InvokeScriptTransaction =>
            (ist.version, ist.bodyBytes(), ist.proofs)
          case _ =>
            (0.toByte, Array.emptyByteArray, Proofs.empty)
        }

        AttachedPaymentExtractor
          .extractPayments(ci, stdLibVersion, blockchain, target)
          .map { payments =>
            Tx.CI(
              Proven(
                Header(ci.id(), ci.fee, ci.timestamp, version),
                RideRecipient.Address(ByteStr(ci.sender.toAddress.bytes)),
                ByteStr(bodyBytes),
                ci.sender,
                proofs.toIndexedSeq
              ),
              toRide(ci.dApp),
              payments,
              ci.feeAssetId.compatId,
              Some(ci.funcCall.function.funcName),
              ci.funcCall.args.map(arg => arg.asInstanceOf[EVALUATED])
            )
          }

      case u: UpdateAssetInfoTransaction =>
        Tx.UpdateAssetInfo(proven(u), u.assetId.id, u.name, u.description).asRight

      case eth: EthereumTransaction =>
        Left(s"Unexpected $eth")
    }

  def mapTransferTx(t: TransferTransactionLike): Tx.Transfer = {
    val (version, bodyBytes, proofs) = t match {
      case tt: TransferTransaction =>
        (tt.version, tt.bodyBytes(), tt.proofs)
      case _ =>
        (0.toByte, Array.emptyByteArray, Proofs.empty)
    }
    Tx.Transfer(
      Proven(
        Header(t.id(), t.fee, t.timestamp, version),
        RideRecipient.Address(ByteStr(t.sender.toAddress.bytes)),
        ByteStr(bodyBytes),
        t.sender,
        proofs.toIndexedSeq
      ),
      feeAssetId = t.feeAssetId.compatId,
      assetId = t.assetId.compatId,
      amount = t.amount.value,
      recipient = toRide(t.recipient),
      attachment = t.attachment
    )
  }

  def toRide(recipient: AddressOrAlias): RideRecipient = recipient match {
    case address: Address => RideRecipient.Address(ByteStr(address.bytes))
    case recipient: Alias => RideRecipient.Alias(recipient.name)
  }
}
