package com.wavesplatform.transaction.protobuf

import java.nio.charset.StandardCharsets

import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.protobuf.utils.PBUtils
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.OrderV1
import com.wavesplatform.transaction.protobuf.Transaction.Data
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.{transaction => vt}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

trait PBTransactionImplicits {
  private[this] val WavesAssetId    = ByteStr.empty
  private[this] val NoFeeAssetId    = WavesAssetId
  private[this] val NoChainId: Byte = 0 // AddressScheme.current.chainId

  implicit class PBTransactionVanillaAdapter(tx: Transaction) extends VanillaTransaction with SignedTransaction with FastHashId {
    def underlying: Transaction = tx

    override def timestamp: Long                   = tx.timestamp
    override val sender: PublicKeyAccount          = tx.sender
    override val proofs                            = Proofs(tx.proofsArray)
    override val signature: ByteStr                = tx.proofs.toSignature
    override def builder: TransactionParser        = Transaction
    override def assetFee: (Option[AssetId], Long) = (Some(tx.feeAssetId).filterNot(_.isEmpty), tx.fee)
    override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(tx.version match {
      case 1 | 2 => tx.toVanilla.bodyBytes()
      case _     => PBUtils.encodeDeterministic(tx.copy(proofsArray = Nil))
    })
    override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(tx.toByteArray)
    override val json: Coeval[JsObject]     = Coeval.evalOnce(Json.toJson(tx).as[JsObject])
  }

  implicit class VanillaOrderImplicitConversionOps(order: vt.assets.exchange.Order) {
    def toPB: ExchangeTransactionData.Order = {
      ExchangeTransactionData.Order(
        order.senderPublicKey,
        order.matcherPublicKey,
        Some(ExchangeTransactionData.Order.AssetPair(order.assetPair.amountAsset, order.assetPair.priceAsset)),
        order.orderType match {
          case vt.assets.exchange.OrderType.BUY  => ExchangeTransactionData.Order.Type.BUY
          case vt.assets.exchange.OrderType.SELL => ExchangeTransactionData.Order.Type.SELL
        },
        order.amount,
        order.price,
        order.timestamp,
        order.expiration,
        order.matcherFee,
        order.proofs,
        order.version
      )
    }
  }

  implicit class PBOrderImplicitConversionOps(order: ExchangeTransactionData.Order) {
    def toVanillaWithVersion(version: Int): vt.assets.exchange.Order = {
      vt.assets.exchange.Order(
        order.senderPublicKey,
        order.matcherPublicKey,
        vt.assets.exchange.AssetPair(order.getAssetPair.amountAssetId, order.getAssetPair.priceAssetId),
        order.orderType match {
          case ExchangeTransactionData.Order.Type.BUY             => vt.assets.exchange.OrderType.BUY
          case ExchangeTransactionData.Order.Type.SELL            => vt.assets.exchange.OrderType.SELL
          case ExchangeTransactionData.Order.Type.Unrecognized(v) => throw new IllegalArgumentException(s"Unknown order type: $v")
        },
        order.amount,
        order.price,
        order.timestamp,
        order.expiration,
        order.matcherFee,
        vt.Proofs(order.proofs),
        version
      )
    }

    def toVanilla: vt.assets.exchange.Order = toVanillaWithVersion(order.version)
  }

  implicit class VanillaTransactionImplicitConversionOps(tx: VanillaTransaction) {
    def toPB: Transaction = tx match {
      // Uses version "2" for "modern" transactions with single version and proofs field
      case MassTransferTransaction(assetId, sender, transfers, timestamp, fee, attachment, proofs) =>
        val data = MassTransferTransactionData(transfers.map(pt => MassTransferTransactionData.Transfer(pt.address, pt.amount)))
        Transaction(assetId, sender, NoChainId, fee, NoFeeAssetId, ByteStr(attachment), timestamp, 2, proofs, Data.MassTransfer(data))

      case vt.GenesisTransaction(recipient, amount, timestamp, signature) =>
        val data = GenesisTransactionData(recipient, amount)
        Transaction(timestamp = timestamp, proofsArray = Seq(signature), data = Data.Genesis(data))

      case vt.transfer.TransferTransactionV1(assetId, sender, recipient, amount, timestamp, feeAssetId, fee, attachment, signature) =>
        val data = TransferTransactionData(recipient, amount)
        Transaction(assetId, sender, NoChainId, fee, feeAssetId, ByteStr(attachment), timestamp, 1, Seq(signature), Data.Transfer(data))

      case vt.transfer.TransferTransactionV2(sender, recipient, assetId, amount, timestamp, feeAssetId, fee, attachment, proofs) =>
        val data = TransferTransactionData(recipient, amount)
        Transaction(assetId, sender, NoChainId, fee, feeAssetId, ByteStr(attachment), timestamp, 2, proofs, Data.Transfer(data))

      case tx @ vt.CreateAliasTransactionV1(sender, alias, fee, timestamp, signature) =>
        val data = CreateAliasTransactionData(alias)
        Transaction(None, sender, NoChainId, fee, tx.assetFee._1, None, timestamp, 1, Seq(signature), Data.CreateAlias(data))

      case tx @ vt.CreateAliasTransactionV2(sender, alias, fee, timestamp, proofs) =>
        val data = CreateAliasTransactionData(alias)
        Transaction(None, sender, NoChainId, fee, tx.assetFee._1, None, timestamp, 2, proofs, Data.CreateAlias(data))

      case tx @ vt.assets.exchange
            .ExchangeTransactionV1(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, signature) =>
        val data = ExchangeTransactionData(Some(buyOrder.toPB), Some(sellOrder.toPB), amount, price, buyMatcherFee, sellMatcherFee)
        Transaction(None, tx.sender, NoChainId, fee, tx.assetFee._1, None, timestamp, 1, Seq(signature), Data.Exchange(data))

      case tx @ vt.assets.exchange.ExchangeTransactionV2(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, proofs) =>
        val data = ExchangeTransactionData(Some(buyOrder.toPB), Some(sellOrder.toPB), amount, price, buyMatcherFee, sellMatcherFee)
        Transaction(None, tx.sender, NoChainId, fee, tx.assetFee._1, None, timestamp, 2, proofs, Data.Exchange(data))

      case vt.assets.IssueTransactionV1(sender, name, description, quantity, decimals, reissuable, fee, timestamp, signature) =>
        val data = IssueTransactionData(new String(name, StandardCharsets.UTF_8),
                                        new String(description, StandardCharsets.UTF_8),
                                        quantity,
                                        decimals,
                                        reissuable,
                                        None)
        Transaction(None, sender, NoChainId, fee, tx.assetFee._1, None, timestamp, 1, Seq(signature), Data.Issue(data))

      case vt.assets.IssueTransactionV2(chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, proofs) =>
        val data = IssueTransactionData(new String(name, StandardCharsets.UTF_8),
                                        new String(description, StandardCharsets.UTF_8),
                                        quantity,
                                        decimals,
                                        reissuable,
                                        script)
        Transaction(None, sender, chainId, fee, tx.assetFee._1, None, timestamp, 2, proofs, Data.Issue(data))

      case tx @ vt.assets.ReissueTransactionV1(sender, assetId, quantity, reissuable, fee, timestamp, signature) =>
        val data = ReissueTransactionData(quantity, reissuable)
        Transaction(assetId,
                    sender,
                    tx.chainByte.getOrElse(NoChainId).toInt,
                    fee,
                    tx.assetFee._1,
                    None,
                    timestamp,
                    1,
                    Seq(signature),
                    Data.Reissue(data))

      case tx @ vt.assets.ReissueTransactionV2(chainId, sender, assetId, quantity, reissuable, fee, timestamp, proofs) =>
        val data = ReissueTransactionData(quantity, reissuable)
        Transaction(assetId, sender, chainId, fee, tx.assetFee._1, None, timestamp, 2, proofs, Data.Reissue(data))

      case tx @ vt.assets.BurnTransactionV1(sender, assetId, quantity, fee, timestamp, signature) =>
        val data = BurnTransactionData(quantity)
        Transaction(assetId,
                    sender,
                    tx.chainByte.getOrElse(NoChainId).toInt,
                    fee,
                    tx.assetFee._1,
                    None,
                    timestamp,
                    1,
                    Seq(signature),
                    Data.Burn(data))

      case tx @ vt.assets.BurnTransactionV2(chainId, sender, assetId, quantity, fee, timestamp, proofs) =>
        val data = BurnTransactionData(quantity)
        Transaction(assetId, sender, chainId, fee, tx.assetFee._1, None, timestamp, 2, proofs, Data.Burn(data))

      case vt.assets.SetAssetScriptTransaction(chainId, sender, assetId, script, fee, timestamp, proofs) =>
        val data = SetAssetScriptTransactionData(script)
        Transaction(assetId, sender, chainId, fee, tx.assetFee._1, None, timestamp, 2, proofs, Data.SetAssetScript(data))

      case vt.smart.SetScriptTransaction(chainId, sender, script, fee, timestamp, proofs) =>
        val data = SetScriptTransactionData(script)
        Transaction(None, sender, chainId, fee, tx.assetFee._1, None, timestamp, 2, proofs, Data.SetScript(data))

      case tx @ vt.lease.LeaseTransactionV1(sender, amount, fee, timestamp, recipient, signature) =>
        val data = LeaseTransactionData(recipient, amount)
        Transaction(None, sender, NoChainId, fee, tx.assetFee._1, None, timestamp, 1, Seq(signature), Data.Lease(data))

      case tx @ vt.lease.LeaseTransactionV2(sender, amount, fee, timestamp, recipient, proofs) =>
        val data = LeaseTransactionData(recipient, amount)
        Transaction(None, sender, NoChainId, fee, tx.assetFee._1, None, timestamp, 2, proofs, Data.Lease(data))

      case tx @ vt.lease.LeaseCancelTransactionV1(sender, leaseId, fee, timestamp, signature) =>
        val data = LeaseCancelTransactionData(leaseId)
        Transaction(None, sender, NoChainId, fee, tx.assetFee._1, None, timestamp, 1, Seq(signature), Data.LeaseCancel(data))

      case tx @ vt.lease.LeaseCancelTransactionV2(chainId, sender, leaseId, fee, timestamp, proofs) =>
        val data = LeaseCancelTransactionData(leaseId)
        Transaction(None, sender, chainId, fee, tx.assetFee._1, None, timestamp, 2, proofs, Data.LeaseCancel(data))

      case tx @ vt.DataTransaction(sender, data, fee, timestamp, proofs) =>
        val txData = DataTransactionData(
          data.map(de =>
            DataTransactionData.DataEntry(
              de.key,
              de match {
                case IntegerDataEntry(_, value) => DataTransactionData.DataEntry.Value.IntValue(value)
                case BooleanDataEntry(_, value) => DataTransactionData.DataEntry.Value.BoolValue(value)
                case BinaryDataEntry(_, value)  => DataTransactionData.DataEntry.Value.BinaryValue(value)
                case StringDataEntry(_, value)  => DataTransactionData.DataEntry.Value.StringValue(value)
              }
          )))
        Transaction(None, sender, NoChainId, fee, tx.assetFee._1, None, timestamp, 2, proofs, Data.DataTransaction(txData))

      case tx @ vt.assets.SponsorFeeTransaction(sender, assetId, minSponsoredAssetFee, fee, timestamp, proofs) =>
        val data = SponsorFeeTransactionData(minSponsoredAssetFee.getOrElse(0L))
        Transaction(assetId, sender, NoChainId, fee, tx.assetFee._1, None, timestamp, 2, proofs, Data.SponsorFee(data))

      case _ =>
        throw new IllegalArgumentException(s"Unsupported transaction: $tx")
    }
  }

  implicit class PBTransactionImplicitConversionOps(tx: Transaction) {
    def toVanilla: VanillaTransaction = tx.data match {
      case Data.MassTransfer(MassTransferTransactionData(transfers)) =>
        vt.transfer.MassTransferTransaction(
          tx.assetId,
          tx.sender,
          transfers.map(t => ParsedTransfer(t.address, t.amount)).toList,
          tx.timestamp,
          tx.fee,
          tx.attachment.arr,
          tx.proofs
        )

      case Data.Genesis(GenesisTransactionData(recipient, amount)) =>
        vt.GenesisTransaction(recipient, amount, tx.timestamp, tx.signature)

      case Data.Transfer(TransferTransactionData(recipient, amount)) =>
        tx.version match {
          case 1 =>
            vt.transfer
              .TransferTransactionV1(tx.assetId, tx.sender, recipient, amount, tx.timestamp, tx.feeAssetId, tx.fee, tx.attachment.arr, tx.signature)
          case 2 =>
            vt.transfer
              .TransferTransactionV2(tx.sender, recipient, tx.assetId, amount, tx.timestamp, tx.feeAssetId, tx.fee, tx.attachment.arr, tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.CreateAlias(CreateAliasTransactionData(alias)) =>
        tx.version match {
          case 1 => vt.CreateAliasTransactionV1(tx.sender, alias, tx.fee, tx.timestamp, tx.signature)
          case 2 => vt.CreateAliasTransactionV2(tx.sender, alias, tx.fee, tx.timestamp, tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Issue(IssueTransactionData(name, description, quantity, decimals, reissuable, script)) =>
        tx.version match {
          case 1 =>
            vt.assets.IssueTransactionV1(
              tx.sender,
              name.getBytes(StandardCharsets.UTF_8),
              description.getBytes(StandardCharsets.UTF_8),
              quantity,
              decimals,
              reissuable,
              tx.fee,
              tx.timestamp,
              tx.signature
            )
          case 2 =>
            vt.assets.IssueTransactionV2(
              tx.chainId,
              tx.sender,
              name.getBytes(StandardCharsets.UTF_8),
              description.getBytes(StandardCharsets.UTF_8),
              quantity,
              decimals,
              reissuable,
              script,
              tx.fee,
              tx.timestamp,
              tx.proofs
            )
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Reissue(ReissueTransactionData(quantity, reissuable)) =>
        tx.version match {
          case 1 => vt.assets.ReissueTransactionV1(tx.sender, tx.assetId, quantity, reissuable, tx.fee, tx.timestamp, tx.signature)
          case 2 => vt.assets.ReissueTransactionV2(tx.chainId, tx.sender, tx.assetId, quantity, reissuable, tx.fee, tx.timestamp, tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Burn(BurnTransactionData(amount)) =>
        tx.version match {
          case 1 => vt.assets.BurnTransactionV1(tx.sender, tx.assetId, amount, tx.fee, tx.timestamp, tx.signature)
          case 2 => vt.assets.BurnTransactionV2(tx.chainId, tx.sender, tx.assetId, amount, tx.fee, tx.timestamp, tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.SetAssetScript(SetAssetScriptTransactionData(script)) =>
        vt.assets.SetAssetScriptTransaction(tx.chainId, tx.sender, tx.assetId, script, tx.fee, tx.timestamp, tx.proofs)

      case Data.SetScript(SetScriptTransactionData(script)) =>
        vt.smart.SetScriptTransaction(tx.chainId, tx.sender, script, tx.fee, tx.timestamp, tx.proofs)

      case Data.Lease(LeaseTransactionData(recipient, amount)) =>
        tx.version match {
          case 1 => vt.lease.LeaseTransactionV1(tx.sender, amount, tx.fee, tx.timestamp, recipient, tx.signature)
          case 2 => vt.lease.LeaseTransactionV2(tx.sender, amount, tx.fee, tx.timestamp, recipient, tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.LeaseCancel(LeaseCancelTransactionData(leaseId)) =>
        tx.version match {
          case 1 => vt.lease.LeaseCancelTransactionV1(tx.sender, leaseId, tx.fee, tx.timestamp, tx.signature)
          case 2 => vt.lease.LeaseCancelTransactionV2(tx.chainId, tx.sender, leaseId, tx.fee, tx.timestamp, tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Exchange(ExchangeTransactionData(Some(buyOrder), Some(sellOrder), amount, price, buyMatcherFee, sellMatcherFee)) =>
        tx.version match {
          case 1 =>
            vt.assets.exchange.ExchangeTransactionV1(
              buyOrder.toVanillaWithVersion(1).asInstanceOf[OrderV1],
              sellOrder.toVanillaWithVersion(1).asInstanceOf[OrderV1],
              amount,
              price,
              buyMatcherFee,
              sellMatcherFee,
              tx.fee,
              tx.timestamp,
              tx.signature
            )
          case 2 =>
            vt.assets.exchange.ExchangeTransactionV2(buyOrder.toVanilla,
                                                     sellOrder.toVanilla,
                                                     amount,
                                                     price,
                                                     buyMatcherFee,
                                                     sellMatcherFee,
                                                     tx.fee,
                                                     tx.timestamp,
                                                     tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.DataTransaction(DataTransactionData(data)) =>
        import DataTransactionData.DataEntry.Value._
        val entries = data.toList.map { de =>
          de.value match {
            case IntValue(num)      => IntegerDataEntry(de.key, num)
            case BoolValue(bool)    => BooleanDataEntry(de.key, bool)
            case BinaryValue(bytes) => BinaryDataEntry(de.key, bytes)
            case StringValue(str)   => StringDataEntry(de.key, str)
            case Empty              => throw new IllegalArgumentException(s"Empty entries not supported: $data")
          }
        }
        vt.DataTransaction(
          tx.sender,
          entries,
          tx.fee,
          tx.timestamp,
          tx.proofs
        )

      case Data.SponsorFee(SponsorFeeTransactionData(minSponsoredAssetFee)) =>
        vt.assets.SponsorFeeTransaction(tx.sender, tx.assetId, Option(minSponsoredAssetFee).filter(_ != 0), tx.fee, tx.timestamp, tx.proofs)

      case data =>
        throw new IllegalArgumentException(s"Unsupported transaction data: $data")
    }
  }

  private[this] implicit def implicitIntToByte(int: Int): Byte = {
    require(int > 0 && int <= 0xFF, s"Byte overflow: $int")
    int.toByte
  }

  private[this] implicit def implicitAssetIdToOption(assetId: AssetId): Option[AssetId] =
    Option(assetId).filterNot(_.isEmpty)

  private[this] implicit def implicitAssetIdOptionToAssetId(assetId: Option[AssetId]): AssetId =
    assetId.getOrElse(WavesAssetId)
}

// object PBTransactionImplicits extends PBTransactionImplicits
