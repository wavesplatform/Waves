package com.wavesplatform.transaction.protobuf
import com.google.protobuf.ByteString
import com.wavesplatform.account.protobuf.PBRecipientImplicits
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.protobuf.PBSerializable._
import com.wavesplatform.serialization.protobuf.{PBMappers, PBSerializable, PBSerializableUnsigned}
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.assets.exchange.OrderV1
import com.wavesplatform.transaction.protobuf.ExchangeTransactionData.{BuySellOrders, Orders}
import com.wavesplatform.transaction.protobuf.Transaction.Data
import com.wavesplatform.transaction.smart.script.ScriptReader
import com.wavesplatform.transaction.smart.script.protobuf.{Script => PBScript}
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{Proofs, _}
import com.wavesplatform.{crypto, transaction => vt}
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.annotation.switch

trait PBSignedTransactionImplicits { self: PBMappers with PBTransactionImplicits with PBAmountImplicits with PBRecipientImplicits =>
  import InternalImplicits._

  implicit def extractTransactionFromSignedTransaction(tx: PBSignedTransaction): PBTransaction = tx.getTransaction

  implicit val PBSignedTransactionPBSerializableInstance = new PBSerializable[PBSignedTransaction] with PBSerializableUnsigned[PBSignedTransaction] {
    override def protoBytes(value: PBSignedTransaction): SerializedT         = PBTransactionSerialization.signedBytes(value)
    override def protoBytesUnsigned(value: PBSignedTransaction): SerializedT = PBTransactionSerialization.unsignedBytes(value.getTransaction)
  }

  class PBSignedTransactionVanillaAdapter(tx: PBSignedTransaction) extends VanillaTransaction with com.wavesplatform.transaction.SignedTransaction {
    def underlying: PBSignedTransaction = tx

    override def builder = PBTransactionParser

    override def timestamp: Long          = tx.getTransaction.timestamp
    override val sender: PublicKeyAccount = PublicKeyAccount(tx.getTransaction.senderPublicKey.toByteArray)
    override val proofs: Proofs           = Proofs.empty
    override val signature: ByteStr       = proofs.toSignature

    override def assetFee: (Option[ByteStr], Long) = tx.getFee.amount match {
      case Amount.Amount.WavesAmount(amount)                       => (None, amount)
      case Amount.Amount.AssetAmount(AssetAmount(assetId, amount)) => (Some(assetId.toByteArray), amount)
      case Amount.Amount.Empty                                     => (None, 0L)
    }

    override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce((tx.version: @switch) match {
      case 1 | 2 => // Legacy
        tx.toVanilla.bodyBytes()

      case _ =>
        tx.protoBytesUnsigned
    })

    override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(tx.protoBytes)

    override val json: Coeval[JsObject] = Coeval.evalOnce((tx.version: @switch) match {
      case 1 | 2 => tx.toVanilla.json()
      case _     => ???
    })

    override val id: Coeval[ByteStr] = Coeval.evalOnce((tx.version: @switch) match {
      case 1 | 2 => // Legacy
        tx.toVanilla.id()

      case _ =>
        // PBUtils.encodeDeterministic(tx.copy(proofsArray = Nil))
        FastHashId.create(bodyBytes())
    })

    override val signatureValid: Coeval[Boolean] = Coeval.evalOnce {
      (tx.data.isGenesis || tx.version > 1) || (tx.toVanilla match {
        case s: Signed => s.signatureValid()
        case _         => true
      })
    }

    override def equals(other: Any): Boolean = other match {
      case a: PBSignedTransactionVanillaAdapter => tx.equals(a.underlying)
      case a: VanillaTransaction                => tx.equals(a.toPB)
      case _                                    => tx.equals(other)
    }

    // private[this] lazy val _hashCode = if (tx.version > 2) tx.hashCode() else tx.toVanilla.hashCode()
    override def hashCode(): Int = tx.hashCode() // _hashCode
  }

  implicit class SignedVanillaTransactionImplicitConversionOps(tx: VanillaTransaction) {
    def toPB: PBSignedTransaction = tx match {
      // Uses version "2" for "modern" transactions with single version and proofs field
      case a: PBSignedTransactionVanillaAdapter =>
        a.underlying

      case vt.GenesisTransaction(recipient, amount, timestamp, signature) =>
        val data = GenesisTransactionData(ByteString.copyFrom(recipient.bytes), amount)
        SignedTransactionFactory.create(sender = PublicKeyAccount(Array.emptyByteArray),
                                        timestamp = timestamp,
                                        version = 1,
                                        data = Data.Genesis(data))

      case vt.PaymentTransaction(sender, recipient, amount, fee, timestamp, signature) =>
        val data = PaymentTransactionData(ByteString.copyFrom(recipient.bytes), amount)
        SignedTransactionFactory.create(sender, ChainId.empty, fee, AssetId.Waves, timestamp, 1, Seq(signature), Data.Payment(data))

      case vt.transfer.TransferTransactionV1(assetId, sender, recipient, amount, timestamp, feeAssetId, fee, attachment, signature) =>
        val data = TransferTransactionData(Some(recipient), Some((assetId, amount)), ByteString.copyFrom(attachment))
        SignedTransactionFactory.create(sender, ChainId.empty, fee, feeAssetId, timestamp, 1, Seq(signature), Data.Transfer(data))

      case vt.transfer.TransferTransactionV2(sender, recipient, assetId, amount, timestamp, feeAssetId, fee, attachment, proofs) =>
        val data = TransferTransactionData(Some(recipient), Some((assetId, amount)), ByteString.copyFrom(attachment))
        SignedTransactionFactory.create(sender, ChainId.empty, fee, feeAssetId, timestamp, 2, proofs, Data.Transfer(data))

      case tx @ vt.CreateAliasTransactionV1(sender, alias, fee, timestamp, signature) =>
        val data = CreateAliasTransactionData(alias.name)
        SignedTransactionFactory.create(sender, ChainId.empty, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.CreateAlias(data))

      case tx @ vt.CreateAliasTransactionV2(sender, alias, fee, timestamp, proofs) =>
        val data = CreateAliasTransactionData(alias.name)
        SignedTransactionFactory.create(sender, ChainId.empty, fee, tx.assetFee._1, timestamp, 2, proofs, Data.CreateAlias(data))

      case tx @ vt.assets.exchange
            .ExchangeTransactionV1(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, signature) =>
        val data = ExchangeTransactionData(amount,
                                           price,
                                           buyMatcherFee,
                                           sellMatcherFee,
                                           Orders.BuySellOrders(BuySellOrders(Some(buyOrder.toPB), Some(sellOrder.toPB))))
        SignedTransactionFactory.create(tx.sender, ChainId.empty, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Exchange(data))

      case tx @ vt.assets.exchange.ExchangeTransactionV2(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, proofs) =>
        val data = ExchangeTransactionData(amount,
                                           price,
                                           buyMatcherFee,
                                           sellMatcherFee,
                                           Orders.BuySellOrders(BuySellOrders(Some(buyOrder.toPB), Some(sellOrder.toPB))))
        SignedTransactionFactory.create(tx.sender, ChainId.empty, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Exchange(data))

      case vt.assets.IssueTransactionV1(sender, name, description, quantity, decimals, reissuable, fee, timestamp, signature) =>
        val data = IssueTransactionData(ByteStr(name), ByteStr(description), quantity, decimals, reissuable, None)
        SignedTransactionFactory.create(sender, ChainId.empty, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Issue(data))

      case vt.assets.IssueTransactionV2(chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, proofs) =>
        val data = IssueTransactionData(ByteStr(name), ByteStr(description), quantity, decimals, reissuable, script.map(s => PBScript(s.bytes())))
        SignedTransactionFactory.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Issue(data))

      case tx @ vt.assets.ReissueTransactionV1(sender, assetId, quantity, reissuable, fee, timestamp, signature) =>
        val data = ReissueTransactionData(Some(AssetAmount(assetId, quantity)), reissuable)
        SignedTransactionFactory.create(sender, tx.chainByte, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Reissue(data))

      case tx @ vt.assets.ReissueTransactionV2(chainId, sender, assetId, amount, reissuable, fee, timestamp, proofs) =>
        val data = ReissueTransactionData(Some(AssetAmount(assetId, amount)), reissuable)
        SignedTransactionFactory.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Reissue(data))

      case tx @ vt.assets.BurnTransactionV1(sender, assetId, amount, fee, timestamp, signature) =>
        val data = BurnTransactionData(Some(AssetAmount(assetId, amount)))
        SignedTransactionFactory.create(sender, tx.chainByte, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Burn(data))

      case tx @ vt.assets.BurnTransactionV2(chainId, sender, assetId, amount, fee, timestamp, proofs) =>
        val data = BurnTransactionData(Some(AssetAmount(assetId, amount)))
        SignedTransactionFactory.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Burn(data))

      case vt.assets.SetAssetScriptTransaction(chainId, sender, assetId, script, fee, timestamp, proofs) =>
        val data = SetAssetScriptTransactionData(assetId, script.map(s => PBScript(s.bytes())))
        SignedTransactionFactory.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.SetAssetScript(data))

      case vt.smart.SetScriptTransaction(chainId, sender, script, fee, timestamp, proofs) =>
        val data = SetScriptTransactionData(script.map(s => PBScript(s.bytes())))
        SignedTransactionFactory.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.SetScript(data))

      case tx @ vt.lease.LeaseTransactionV1(sender, amount, fee, timestamp, recipient, signature) =>
        val data = LeaseTransactionData(Some(recipient), amount)
        SignedTransactionFactory.create(sender, ChainId.empty, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Lease(data))

      case tx @ vt.lease.LeaseTransactionV2(sender, amount, fee, timestamp, recipient, proofs) =>
        val data = LeaseTransactionData(Some(recipient), amount)
        SignedTransactionFactory.create(sender, ChainId.empty, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Lease(data))

      case tx @ vt.lease.LeaseCancelTransactionV1(sender, leaseId, fee, timestamp, signature) =>
        val data = LeaseCancelTransactionData(leaseId)
        SignedTransactionFactory.create(sender, ChainId.empty, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.LeaseCancel(data))

      case tx @ vt.lease.LeaseCancelTransactionV2(chainId, sender, leaseId, fee, timestamp, proofs) =>
        val data = LeaseCancelTransactionData(leaseId)
        SignedTransactionFactory.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.LeaseCancel(data))

      case tx @ MassTransferTransaction(assetId, sender, transfers, timestamp, fee, attachment, proofs) =>
        val data = MassTransferTransactionData(
          ByteString.copyFrom(assetId.getOrElse(ByteStr.empty)),
          transfers.map(pt => MassTransferTransactionData.Transfer(Some(pt.address), pt.amount)),
          attachment: ByteStr
        )
        SignedTransactionFactory.create(sender, ChainId.empty, fee, tx.assetFee._1, timestamp, 2, proofs, Data.MassTransfer(data))

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
        SignedTransactionFactory.create(sender, ChainId.empty, fee, tx.assetFee._1, timestamp, 2, proofs, Data.DataTransaction(txData))

      case tx @ vt.assets.SponsorFeeTransaction(sender, assetId, minSponsoredAssetFee, fee, timestamp, proofs) =>
        val data = SponsorFeeTransactionData(Some(AssetAmount(assetId, minSponsoredAssetFee.getOrElse(0L))))
        SignedTransactionFactory.create(sender, ChainId.empty, fee, tx.assetFee._1, timestamp, 2, proofs, Data.SponsorFee(data))

      case _ =>
        throw new IllegalArgumentException(s"Unsupported transaction: $tx")
    }
  }

  implicit class PBSignedTransactionImplicitConversionOps(tx: PBSignedTransaction) {
    def signature: ByteStr = tx.proofs.headOption.getOrElse(ByteString.EMPTY).toByteArray

    def isLegacy: Boolean = tx.version == 1 || tx.version == 2

    def toVanillaOrAdapter: VanillaTransaction = if (this.isLegacy) toVanilla else toVanillaAdapter

    def toVanillaAdapter = new PBSignedTransactionVanillaAdapter(tx)

    def toVanilla: VanillaTransaction = tx.data match {
      case Data.Genesis(GenesisTransactionData(recipient, amount)) =>
        vt.GenesisTransaction(Address.fromBytes(recipient.toByteArray).right.get, amount, tx.timestamp, signature)

      case Data.Payment(PaymentTransactionData(recipient, amount)) =>
        vt.PaymentTransaction(tx.senderPublicKey.publicKeyAccount,
                              Address.fromBytes(recipient.toByteArray).right.get,
                              amount,
                              tx.getFee,
                              tx.timestamp,
                              signature)

      case Data.Transfer(TransferTransactionData(Some(recipient), Some(amount), attachment)) =>
        tx.version match {
          case 1 =>
            vt.transfer
              .TransferTransactionV1(
                amount.assetId,
                tx.senderPublicKey.publicKeyAccount,
                recipient.toAddressOrAlias,
                amount,
                tx.timestamp,
                tx.getFee.assetId,
                tx.getFee,
                attachment.toByteArray,
                signature
              )
          case 2 =>
            vt.transfer
              .TransferTransactionV2(
                tx.senderPublicKey.publicKeyAccount,
                recipient.toAddressOrAlias,
                amount.assetId,
                amount,
                tx.timestamp,
                tx.getFee.assetId,
                tx.getFee,
                attachment.toByteArray,
                tx.proofs
              )
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.CreateAlias(CreateAliasTransactionData(alias)) =>
        import com.wavesplatform.common.utils._
        tx.version match {
          case 1 =>
            vt.CreateAliasTransactionV1(
              tx.senderPublicKey.publicKeyAccount,
              com.wavesplatform.account.Alias.buildAlias(tx.chainId: ChainId, alias).explicitGet(),
              tx.getFee,
              tx.timestamp,
              signature
            )
          case 2 =>
            vt.CreateAliasTransactionV2(
              tx.senderPublicKey.publicKeyAccount,
              com.wavesplatform.account.Alias.buildAlias(tx.chainId: ChainId, alias).explicitGet(),
              tx.getFee,
              tx.timestamp,
              tx.proofs
            )
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Issue(IssueTransactionData(name, description, quantity, decimals, reissuable, script)) =>
        tx.version match {
          case 1 =>
            vt.assets.IssueTransactionV1(
              tx.senderPublicKey.publicKeyAccount,
              name.toByteArray,
              description.toByteArray,
              quantity,
              decimals.toByte,
              reissuable,
              tx.getFee,
              tx.timestamp,
              signature
            )
          case 2 =>
            vt.assets.IssueTransactionV2(
              tx.chainId: ChainId,
              tx.senderPublicKey.publicKeyAccount,
              name.toByteArray,
              description.toByteArray,
              quantity,
              decimals.toByte,
              reissuable,
              script.map(s => ScriptReader.fromBytes(s.bytes.toByteArray).right.get),
              tx.getFee,
              tx.timestamp,
              tx.proofs
            )
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Reissue(ReissueTransactionData(Some(AssetAmount(assetId, amount)), reissuable)) =>
        tx.version match {
          case 1 =>
            vt.assets.ReissueTransactionV1(tx.senderPublicKey.publicKeyAccount, assetId, amount, reissuable, tx.getFee, tx.timestamp, signature)
          case 2 =>
            vt.assets.ReissueTransactionV2(tx.chainId: ChainId,
                                           tx.senderPublicKey.publicKeyAccount,
                                           assetId,
                                           amount,
                                           reissuable,
                                           tx.getFee,
                                           tx.timestamp,
                                           tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Burn(BurnTransactionData(Some(AssetAmount(assetId, amount)))) =>
        tx.version match {
          case 1 => vt.assets.BurnTransactionV1(tx.senderPublicKey.publicKeyAccount, assetId, amount, tx.getFee, tx.timestamp, signature)
          case 2 =>
            vt.assets.BurnTransactionV2(tx.chainId: ChainId, tx.senderPublicKey.publicKeyAccount, assetId, amount, tx.getFee, tx.timestamp, tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.SetAssetScript(SetAssetScriptTransactionData(assetId, script)) =>
        vt.assets.SetAssetScriptTransaction(
          tx.chainId: ChainId,
          tx.senderPublicKey.publicKeyAccount,
          assetId,
          script.map(s => ScriptReader.fromBytes(s.bytes.toByteArray).right.get),
          tx.getFee,
          tx.timestamp,
          tx.proofs
        )

      case Data.SetScript(SetScriptTransactionData(script)) =>
        vt.smart.SetScriptTransaction(
          tx.chainId: ChainId,
          tx.senderPublicKey.publicKeyAccount,
          script.map(s => ScriptReader.fromBytes(s.bytes.toByteArray).right.get),
          tx.getFee,
          tx.timestamp,
          tx.proofs
        )

      case Data.Lease(LeaseTransactionData(Some(recipient), amount)) =>
        tx.version match {
          case 1 =>
            vt.lease.LeaseTransactionV1(tx.senderPublicKey.publicKeyAccount, amount, tx.getFee, tx.timestamp, recipient.toAddressOrAlias, signature)
          case 2 =>
            vt.lease.LeaseTransactionV2(tx.senderPublicKey.publicKeyAccount, amount, tx.getFee, tx.timestamp, recipient.toAddressOrAlias, tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.LeaseCancel(LeaseCancelTransactionData(leaseId)) =>
        tx.version match {
          case 1 => vt.lease.LeaseCancelTransactionV1(tx.senderPublicKey.publicKeyAccount, leaseId.byteStr, tx.getFee, tx.timestamp, signature)
          case 2 =>
            vt.lease.LeaseCancelTransactionV2(tx.chainId: ChainId,
                                              tx.senderPublicKey.publicKeyAccount,
                                              leaseId.toByteArray,
                                              tx.getFee,
                                              tx.timestamp,
                                              tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Exchange(
          ExchangeTransactionData(amount,
                                  price,
                                  buyMatcherFee,
                                  sellMatcherFee,
                                  Orders.BuySellOrders(BuySellOrders(Some(buyOrder), Some(sellOrder))))) =>
        tx.version match {
          case 1 =>
            vt.assets.exchange.ExchangeTransactionV1(
              buyOrder.toVanillaWithVersion(1).asInstanceOf[OrderV1],
              sellOrder.toVanillaWithVersion(1).asInstanceOf[OrderV1],
              amount,
              price,
              buyMatcherFee,
              sellMatcherFee,
              tx.getFee,
              tx.timestamp,
              signature
            )
          case 2 =>
            vt.assets.exchange.ExchangeTransactionV2(buyOrder.toVanilla,
                                                     sellOrder.toVanilla,
                                                     amount,
                                                     price,
                                                     buyMatcherFee,
                                                     sellMatcherFee,
                                                     tx.getFee,
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
            case BinaryValue(bytes) => BinaryDataEntry(de.key, bytes.toByteArray)
            case StringValue(str)   => StringDataEntry(de.key, str)
            case Empty              => throw new IllegalArgumentException(s"Empty entries not supported: $data")
          }
        }
        vt.DataTransaction(
          PublicKeyAccount(tx.senderPublicKey.toByteArray),
          entries,
          tx.getFee,
          tx.timestamp,
          tx.proofs
        )

      case Data.MassTransfer(MassTransferTransactionData(assetId, transfers, attachment)) =>
        vt.transfer.MassTransferTransaction(
          Some(assetId.toByteArray: ByteStr).filterNot(_.isEmpty),
          PublicKeyAccount(tx.senderPublicKey.toByteArray),
          transfers.map(t => ParsedTransfer(t.getAddress.toAddressOrAlias, t.amount)).toList,
          tx.timestamp,
          tx.getFee,
          attachment.toByteArray,
          tx.proofs
        )

      case Data.SponsorFee(SponsorFeeTransactionData(Some(AssetAmount(assetId, minFee)))) =>
        vt.assets.SponsorFeeTransaction(PublicKeyAccount(tx.senderPublicKey.toByteArray),
                                        assetId.toByteArray,
                                        Option(minFee).filter(_ > 0),
                                        tx.getFee,
                                        tx.timestamp,
                                        tx.proofs)

      case data =>
        throw new IllegalArgumentException(s"Unsupported transaction data: $data")
    }

    def signed(signer: Array[Byte]): PBSignedTransaction = {
      tx.withProofs(Seq(ByteStr(crypto.sign(signer, tx.toVanillaAdapter.bodyBytes()))))
    }
  }

  private[this] object InternalImplicits {
    implicit def pbBytesSeqToByteStrs(bs: Seq[ByteString]): Seq[ByteStr] = bs.map(bs => bs.toByteArray: ByteStr)
    implicit def pbBytesSeqToProofs(bs: Seq[ByteString]): Proofs         = Proofs(bs.map(bs => bs.toByteArray: ByteStr))
    implicit def byteStrSeqToPBBytes(bs: Seq[ByteStr]): Seq[ByteString]  = bs.map(bs => bs: ByteString)
  }

  private[this] object SignedTransactionFactory {
    def create(sender: com.wavesplatform.account.PublicKeyAccount = PublicKeyAccount.empty,
               chainId: com.wavesplatform.transaction.protobuf.ChainId = ChainId.empty,
               fee: _root_.scala.Long = 0L,
               feeAssetId: com.wavesplatform.transaction.protobuf.AssetId = AssetId.Waves,
               timestamp: _root_.scala.Long = 0L,
               version: _root_.scala.Int = 0,
               proofsArray: _root_.scala.collection.Seq[com.wavesplatform.common.state.ByteStr] = Nil,
               data: com.wavesplatform.transaction.protobuf.Transaction.Data = com.wavesplatform.transaction.protobuf.Transaction.Data.Empty)
      : SignedTransaction = {
      new SignedTransaction(Some(Transaction(chainId, sender.publicKey: ByteStr, Some((feeAssetId, fee): Amount), timestamp, version, data)),
                            proofsArray.map(bs => bs: ByteString))
    }
  }
}
