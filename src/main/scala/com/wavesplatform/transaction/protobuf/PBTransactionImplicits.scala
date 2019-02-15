package com.wavesplatform.transaction.protobuf

import com.google.protobuf.CodedOutputStream
import com.wavesplatform.account.protobuf.Recipient
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.OrderV1
import com.wavesplatform.transaction.protobuf.Transaction.Body.Data
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.{transaction => vt}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.annotation.switch

trait PBTransactionImplicits {
  private[this] val NoChainId: Byte = 0 // AddressScheme.current.chainId

  implicit class PBTransactionVanillaAdapter(tx: PBTransaction) extends VanillaTransaction with SignedTransaction {
    def underlying: Transaction = tx

    override def timestamp: Long                   = tx.timestamp
    override val sender: PublicKeyAccount          = tx.sender
    override val proofs: Proofs                    = Proofs(tx.proofsArray)
    override val signature: ByteStr                = proofs.toSignature
    override def builder: PBTransaction.type       = PBTransaction
    override def assetFee: (Option[ByteStr], Long) = (Some(tx.feeAssetId: ByteStr).filterNot(_.isEmpty), tx.fee)

    override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce((tx.getBody.version: @switch) match {
      case 1 | 2 => // Legacy
        tx.toVanilla.bodyBytes()

      case _ =>
        // PBUtils.encodeDeterministic(tx.copy(proofsArray = Nil))
        encodePBTXWithPrefix(tx)
    })

    override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
      val outArray     = new Array[Byte](tx.serializedSize + 2)
      val outputStream = CodedOutputStream.newInstance(outArray)
      outputStream.write(builder.typeId)
      outputStream.write(builder.version)
      tx.writeTo(outputStream)
      outputStream.checkNoSpaceLeft()
      outArray
    }

    override val json: Coeval[JsObject] = Coeval.evalOnce((tx.getBody.version: @switch) match {
      case 1 | 2 => tx.toVanilla.json()
      case _     => Json.toJson(tx).as[JsObject]
    })

    override val id: Coeval[ByteStr] = Coeval.evalOnce((tx.getBody.version: @switch) match {
      case 1 | 2 => // Legacy
        tx.toVanilla.id()

      case _ =>
        // PBUtils.encodeDeterministic(tx.copy(proofsArray = Nil))
        FastHashId.create(bodyBytes())
    })

    override val signatureValid: Coeval[Boolean] = Coeval.evalOnce {
      (tx.data.isGenesis || tx.version > 1) || this.verifySignature()
    }

    override def equals(other: Any): Boolean = other match {
      case a: PBTransactionVanillaAdapter => tx.equals(a.underlying)
      case a: VanillaTransaction          => tx.equals(a.toPB)
      case _                              => tx.equals(other)
    }

    // private[this] lazy val _hashCode = if (tx.version > 2) tx.hashCode() else tx.toVanilla.hashCode()
    override def hashCode(): Int = tx.hashCode() // _hashCode
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
      case a: PBTransactionVanillaAdapter =>
        a.underlying

      case vt.GenesisTransaction(recipient, amount, timestamp, signature) =>
        val data = GenesisTransactionData(recipient, amount)
        Transaction.create(PublicKeyAccount(Array.emptyByteArray),
                           timestamp = timestamp,
                           version = 1,
                           data = Data.Genesis(data),
                           proofsArray = Seq(signature))

      case vt.PaymentTransaction(sender, recipient, amount, fee, timestamp, signature) =>
        val data = PaymentTransactionData(recipient, amount)
        Transaction.create(sender, NoChainId, fee, AssetId.Waves, timestamp, 1, Data.Payment(data), Seq(signature))

      case vt.transfer.TransferTransactionV1(assetId, sender, recipient, amount, timestamp, feeAssetId, fee, attachment, signature) =>
        val data = TransferTransactionData(assetId, recipient, amount, ByteStr(attachment))
        Transaction.create(sender, NoChainId, fee, feeAssetId, timestamp, 1, Data.Transfer(data), Seq(signature))

      case vt.transfer.TransferTransactionV2(sender, recipient, assetId, amount, timestamp, feeAssetId, fee, attachment, proofs) =>
        val data = TransferTransactionData(assetId, recipient, amount, attachment)
        Transaction.create(sender, NoChainId, fee, feeAssetId, timestamp, 2, Data.Transfer(data), proofs)

      case tx @ vt.CreateAliasTransactionV1(sender, alias, fee, timestamp, signature) =>
        val data = CreateAliasTransactionData(alias)
        Transaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Data.CreateAlias(data), Seq(signature))

      case tx @ vt.CreateAliasTransactionV2(sender, alias, fee, timestamp, proofs) =>
        val data = CreateAliasTransactionData(alias)
        Transaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, Data.CreateAlias(data), proofs)

      case tx @ vt.assets.exchange
            .ExchangeTransactionV1(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, signature) =>
        val data = ExchangeTransactionData(Some(buyOrder.toPB), Some(sellOrder.toPB), amount, price, buyMatcherFee, sellMatcherFee)
        Transaction.create(tx.sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Data.Exchange(data), Seq(signature))

      case tx @ vt.assets.exchange.ExchangeTransactionV2(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, proofs) =>
        val data = ExchangeTransactionData(Some(buyOrder.toPB), Some(sellOrder.toPB), amount, price, buyMatcherFee, sellMatcherFee)
        Transaction.create(tx.sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, Data.Exchange(data), proofs)

      case vt.assets.IssueTransactionV1(sender, name, description, quantity, decimals, reissuable, fee, timestamp, signature) =>
        val data = IssueTransactionData(ByteStr(name), ByteStr(description), quantity, decimals, reissuable, None)
        Transaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Data.Issue(data), Seq(signature))

      case vt.assets.IssueTransactionV2(chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, proofs) =>
        val data = IssueTransactionData(ByteStr(name), ByteStr(description), quantity, decimals, reissuable, script)
        Transaction.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, Data.Issue(data), proofs)

      case tx @ vt.assets.ReissueTransactionV1(sender, assetId, quantity, reissuable, fee, timestamp, signature) =>
        val data = ReissueTransactionData(assetId, quantity, reissuable)
        Transaction.create(sender, tx.chainByte.getOrElse(NoChainId).toInt, fee, tx.assetFee._1, timestamp, 1, Data.Reissue(data), Seq(signature))

      case tx @ vt.assets.ReissueTransactionV2(chainId, sender, assetId, amount, reissuable, fee, timestamp, proofs) =>
        val data = ReissueTransactionData(assetId, amount, reissuable)
        Transaction.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, Data.Reissue(data), proofs)

      case tx @ vt.assets.BurnTransactionV1(sender, assetId, amount, fee, timestamp, signature) =>
        val data = BurnTransactionData(assetId, amount)
        Transaction.create(sender, tx.chainByte.getOrElse(NoChainId).toInt, fee, tx.assetFee._1, timestamp, 1, Data.Burn(data), Seq(signature))

      case tx @ vt.assets.BurnTransactionV2(chainId, sender, assetId, amount, fee, timestamp, proofs) =>
        val data = BurnTransactionData(assetId, amount)
        Transaction.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, Data.Burn(data), proofs)

      case vt.assets.SetAssetScriptTransaction(chainId, sender, assetId, script, fee, timestamp, proofs) =>
        val data = SetAssetScriptTransactionData(assetId, script)
        Transaction.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, Data.SetAssetScript(data), proofs)

      case vt.smart.SetScriptTransaction(chainId, sender, script, fee, timestamp, proofs) =>
        val data = SetScriptTransactionData(script)
        Transaction.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, Data.SetScript(data), proofs)

      case tx @ vt.lease.LeaseTransactionV1(sender, amount, fee, timestamp, recipient, signature) =>
        val data = LeaseTransactionData(recipient, amount)
        Transaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Data.Lease(data), Seq(signature))

      case tx @ vt.lease.LeaseTransactionV2(sender, amount, fee, timestamp, recipient, proofs) =>
        val data = LeaseTransactionData(recipient, amount)
        Transaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, Data.Lease(data), proofs)

      case tx @ vt.lease.LeaseCancelTransactionV1(sender, leaseId, fee, timestamp, signature) =>
        val data = LeaseCancelTransactionData(leaseId)
        Transaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Data.LeaseCancel(data), Seq(signature))

      case tx @ vt.lease.LeaseCancelTransactionV2(chainId, sender, leaseId, fee, timestamp, proofs) =>
        val data = LeaseCancelTransactionData(leaseId)
        Transaction.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, Data.LeaseCancel(data), proofs)

      case tx @ MassTransferTransaction(assetId, sender, transfers, timestamp, fee, attachment, proofs) =>
        val data = MassTransferTransactionData(assetId, transfers.map(pt => MassTransferTransactionData.Transfer(pt.address, pt.amount)), attachment)
        Transaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, Data.MassTransfer(data), proofs)

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
        Transaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, Data.DataTransaction(txData), proofs)

      case tx @ vt.assets.SponsorFeeTransaction(sender, assetId, minSponsoredAssetFee, fee, timestamp, proofs) =>
        val data = SponsorFeeTransactionData(assetId, minSponsoredAssetFee.getOrElse(0L))
        Transaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, Data.SponsorFee(data), proofs)

      case _ =>
        throw new IllegalArgumentException(s"Unsupported transaction: $tx")
    }
  }

  implicit class PBTransactionImplicitConversionOps(tx: Transaction) {
    def isLegacy: Boolean = tx.version == 1 || tx.version == 2

    def toVanillaOrAdapter: VanillaTransaction = if (this.isLegacy) toVanilla else toVanillaAdapter

    def toVanillaAdapter = PBTransactionVanillaAdapter(tx)

    def toVanilla: VanillaTransaction = tx.data match {
      case Data.Genesis(GenesisTransactionData(Some(recipient), amount)) =>
        vt.GenesisTransaction(recipient.toAddress, amount, tx.timestamp, tx.signature)

      case Data.Payment(PaymentTransactionData(Some(recipient), amount)) =>
        vt.PaymentTransaction(tx.sender, recipient.toAddress, amount, tx.fee, tx.timestamp, tx.signature)

      case Data.Transfer(TransferTransactionData(assetId, Some(recipient), amount, attachment)) =>
        tx.version match {
          case 1 =>
            vt.transfer
              .TransferTransactionV1(assetId,
                                     tx.sender,
                                     recipient.toAddressOrAlias,
                                     amount,
                                     tx.timestamp,
                                     tx.feeAssetId,
                                     tx.fee,
                                     attachment,
                                     tx.signature)
          case 2 =>
            vt.transfer
              .TransferTransactionV2(tx.sender,
                                     recipient.toAddressOrAlias,
                                     assetId,
                                     amount,
                                     tx.timestamp,
                                     tx.feeAssetId,
                                     tx.fee,
                                     attachment,
                                     tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.CreateAlias(CreateAliasTransactionData(Some(alias))) =>
        tx.version match {
          case 1 => vt.CreateAliasTransactionV1(tx.sender, alias.toAlias, tx.fee, tx.timestamp, tx.signature)
          case 2 => vt.CreateAliasTransactionV2(tx.sender, alias.toAlias, tx.fee, tx.timestamp, tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Issue(IssueTransactionData(name, description, quantity, decimals, reissuable, script)) =>
        tx.version match {
          case 1 =>
            vt.assets.IssueTransactionV1(
              tx.sender,
              name,
              description,
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
              name,
              description,
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

      case Data.Reissue(ReissueTransactionData(assetId, amount, reissuable)) =>
        tx.version match {
          case 1 => vt.assets.ReissueTransactionV1(tx.sender, assetId, amount, reissuable, tx.fee, tx.timestamp, tx.signature)
          case 2 => vt.assets.ReissueTransactionV2(tx.chainId, tx.sender, assetId, amount, reissuable, tx.fee, tx.timestamp, tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Burn(BurnTransactionData(assetId, amount)) =>
        tx.version match {
          case 1 => vt.assets.BurnTransactionV1(tx.sender, assetId, amount, tx.fee, tx.timestamp, tx.signature)
          case 2 => vt.assets.BurnTransactionV2(tx.chainId, tx.sender, assetId, amount, tx.fee, tx.timestamp, tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.SetAssetScript(SetAssetScriptTransactionData(assetId, script)) =>
        vt.assets.SetAssetScriptTransaction(tx.chainId, tx.sender, assetId, script, tx.fee, tx.timestamp, tx.proofs)

      case Data.SetScript(SetScriptTransactionData(script)) =>
        vt.smart.SetScriptTransaction(tx.chainId, tx.sender, script, tx.fee, tx.timestamp, tx.proofs)

      case Data.Lease(LeaseTransactionData(Some(recipient), amount)) =>
        tx.version match {
          case 1 => vt.lease.LeaseTransactionV1(tx.sender, amount, tx.fee, tx.timestamp, recipient.toAddressOrAlias, tx.signature)
          case 2 => vt.lease.LeaseTransactionV2(tx.sender, amount, tx.fee, tx.timestamp, recipient.toAddressOrAlias, tx.proofs)
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

      case Data.MassTransfer(MassTransferTransactionData(assetId, transfers, attachment)) =>
        vt.transfer.MassTransferTransaction(
          assetId,
          tx.sender,
          transfers.map(t => ParsedTransfer(t.getAddress.toAddressOrAlias, t.amount)).toList,
          tx.timestamp,
          tx.fee,
          attachment,
          tx.proofs
        )

      case Data.SponsorFee(SponsorFeeTransactionData(assetId, minSponsoredAssetFee)) =>
        vt.assets.SponsorFeeTransaction(tx.sender, assetId, Option(minSponsoredAssetFee).filter(_ != 0), tx.fee, tx.timestamp, tx.proofs)

      case data =>
        throw new IllegalArgumentException(s"Unsupported transaction data: $data")
    }
  }

  private[wavesplatform] def encodePBTXWithPrefix(tx: PBTransaction): Array[Byte] = {
    val prefixLength = 3
    val outArray     = new Array[Byte](tx.serializedSize + prefixLength)
    val outputStream = CodedOutputStream.newInstance(outArray)
    outputStream.useDeterministicSerialization() // Do not remove
    outputStream.write('W'.toByte)
    outputStream.write('T'.toByte)
    outputStream.write(tx.chainId: Byte)
    tx.withProofsArray(Nil).writeTo(outputStream)
    outputStream.checkNoSpaceLeft()
    outArray
  }

  private[this] implicit def implicitIntToByte(int: Int): Byte = {
    require(int >= 0 && int <= 0xFF, s"Byte overflow: $int")
    int.toByte
  }

  private[this] implicit def implicitAssetIdToOption(assetId: PBAssetId): Option[VanillaAssetId] =
    Option(assetId).filterNot(_.isEmpty).map(_.bytes)

  private[this] implicit def implicitAssetIdOptionToAssetId(assetId: Option[VanillaAssetId]): PBAssetId =
    assetId.fold(PBAssetId.Waves)(PBAssetId.fromBytes)

  private[this] implicit def implicitAddressToRecipientOption(address: Address): Option[Recipient] =
    Some(address: Recipient)
}

// object PBTransactionImplicits extends PBTransactionImplicits
