package com.wavesplatform.transaction.protobuf
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.account.protobuf.RecipientMessage._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.assets.exchange.OrderV1
import com.wavesplatform.transaction.protobuf.PBTransaction._
import com.wavesplatform.transaction.protobuf.Transaction.Data
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{Proofs, _}
import com.wavesplatform.{crypto, transaction => vt}
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.annotation.switch

trait PBSignedTransactionImplicits {
  val NoChainId = ChainId.empty

  implicit def extractTransactionFromSignedTransaction(tx: PBSignedTransaction): PBTransaction = tx.transaction

  class PBSignedTransactionVanillaAdapter(tx: PBSignedTransaction) extends VanillaTransaction with com.wavesplatform.transaction.SignedTransaction {
    def underlying: PBSignedTransaction = tx

    override def timestamp: Long                   = tx.transaction.timestamp
    override val sender: PublicKeyAccount          = tx.transaction.sender
    override val proofs: Proofs                    = Proofs.empty
    override val signature: ByteStr                = proofs.toSignature
    override def builder: PBTransaction.type       = PBTransaction
    override def assetFee: (Option[ByteStr], Long) = (Some(tx.feeAssetId: ByteStr).filterNot(_.isEmpty), tx.fee)

    override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce((tx.version: @switch) match {
      case 1 | 2 => // Legacy
        tx.toVanilla.bodyBytes()

      case _ =>
        tx.protoUnsignedBytes()
    })

    override val bytes: Coeval[Array[Byte]] = tx.protoBytes

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
        val data = GenesisTransactionData(recipient, amount)
        SignedTransaction.create(sender = PublicKeyAccount(Array.emptyByteArray), timestamp = timestamp, version = 1, data = Data.Genesis(data))

      case vt.PaymentTransaction(sender, recipient, amount, fee, timestamp, signature) =>
        val data = PaymentTransactionData(recipient, amount)
        SignedTransaction.create(sender, NoChainId, fee, AssetId.Waves, timestamp, 1, Seq(signature), Data.Payment(data))

      case vt.transfer.TransferTransactionV1(assetId, sender, recipient, amount, timestamp, feeAssetId, fee, attachment, signature) =>
        val data = TransferTransactionData(assetId, recipient, amount, ByteStr(attachment))
        SignedTransaction.create(sender, NoChainId, fee, feeAssetId, timestamp, 1, Seq(signature), Data.Transfer(data))

      case vt.transfer.TransferTransactionV2(sender, recipient, assetId, amount, timestamp, feeAssetId, fee, attachment, proofs) =>
        val data = TransferTransactionData(assetId, recipient, amount, attachment)
        SignedTransaction.create(sender, NoChainId, fee, feeAssetId, timestamp, 2, proofs, Data.Transfer(data))

      case tx @ vt.CreateAliasTransactionV1(sender, alias, fee, timestamp, signature) =>
        val data = CreateAliasTransactionData(alias)
        SignedTransaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.CreateAlias(data))

      case tx @ vt.CreateAliasTransactionV2(sender, alias, fee, timestamp, proofs) =>
        val data = CreateAliasTransactionData(alias)
        SignedTransaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.CreateAlias(data))

      case tx @ vt.assets.exchange
            .ExchangeTransactionV1(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, signature) =>
        val data = ExchangeTransactionData(Some(buyOrder.toPB), Some(sellOrder.toPB), amount, price, buyMatcherFee, sellMatcherFee)
        SignedTransaction.create(tx.sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Exchange(data))

      case tx @ vt.assets.exchange.ExchangeTransactionV2(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, proofs) =>
        val data = ExchangeTransactionData(Some(buyOrder.toPB), Some(sellOrder.toPB), amount, price, buyMatcherFee, sellMatcherFee)
        SignedTransaction.create(tx.sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Exchange(data))

      case vt.assets.IssueTransactionV1(sender, name, description, quantity, decimals, reissuable, fee, timestamp, signature) =>
        val data = IssueTransactionData(ByteStr(name), ByteStr(description), quantity, decimals, reissuable, None)
        SignedTransaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Issue(data))

      case vt.assets.IssueTransactionV2(chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, proofs) =>
        val data = IssueTransactionData(ByteStr(name), ByteStr(description), quantity, decimals, reissuable, script)
        SignedTransaction.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Issue(data))

      case tx @ vt.assets.ReissueTransactionV1(sender, assetId, quantity, reissuable, fee, timestamp, signature) =>
        val data = ReissueTransactionData(assetId, quantity, reissuable)
        SignedTransaction.create(sender, tx.chainByte, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Reissue(data))

      case tx @ vt.assets.ReissueTransactionV2(chainId, sender, assetId, amount, reissuable, fee, timestamp, proofs) =>
        val data = ReissueTransactionData(assetId, amount, reissuable)
        SignedTransaction.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Reissue(data))

      case tx @ vt.assets.BurnTransactionV1(sender, assetId, amount, fee, timestamp, signature) =>
        val data = BurnTransactionData(assetId, amount)
        SignedTransaction.create(sender, tx.chainByte, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Burn(data))

      case tx @ vt.assets.BurnTransactionV2(chainId, sender, assetId, amount, fee, timestamp, proofs) =>
        val data = BurnTransactionData(assetId, amount)
        SignedTransaction.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Burn(data))

      case vt.assets.SetAssetScriptTransaction(chainId, sender, assetId, script, fee, timestamp, proofs) =>
        val data = SetAssetScriptTransactionData(assetId, script)
        SignedTransaction.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.SetAssetScript(data))

      case vt.smart.SetScriptTransaction(chainId, sender, script, fee, timestamp, proofs) =>
        val data = SetScriptTransactionData(script)
        SignedTransaction.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.SetScript(data))

      case tx @ vt.lease.LeaseTransactionV1(sender, amount, fee, timestamp, recipient, signature) =>
        val data = LeaseTransactionData(recipient, amount)
        SignedTransaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.Lease(data))

      case tx @ vt.lease.LeaseTransactionV2(sender, amount, fee, timestamp, recipient, proofs) =>
        val data = LeaseTransactionData(recipient, amount)
        SignedTransaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.Lease(data))

      case tx @ vt.lease.LeaseCancelTransactionV1(sender, leaseId, fee, timestamp, signature) =>
        val data = LeaseCancelTransactionData(leaseId)
        SignedTransaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 1, Seq(signature), Data.LeaseCancel(data))

      case tx @ vt.lease.LeaseCancelTransactionV2(chainId, sender, leaseId, fee, timestamp, proofs) =>
        val data = LeaseCancelTransactionData(leaseId)
        SignedTransaction.create(sender, chainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.LeaseCancel(data))

      case tx @ MassTransferTransaction(assetId, sender, transfers, timestamp, fee, attachment, proofs) =>
        val data = MassTransferTransactionData(assetId, transfers.map(pt => MassTransferTransactionData.Transfer(pt.address, pt.amount)), attachment)
        SignedTransaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.MassTransfer(data))

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
        SignedTransaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.DataTransaction(txData))

      case tx @ vt.assets.SponsorFeeTransaction(sender, assetId, minSponsoredAssetFee, fee, timestamp, proofs) =>
        val data = SponsorFeeTransactionData(assetId, minSponsoredAssetFee.getOrElse(0L))
        SignedTransaction.create(sender, NoChainId, fee, tx.assetFee._1, timestamp, 2, proofs, Data.SponsorFee(data))

      case _ =>
        throw new IllegalArgumentException(s"Unsupported transaction: $tx")
    }
  }

  implicit class PBTransactionImplicitConversionOps(tx: PBSignedTransaction) {
    def signature: ByteStr = tx.proofs.headOption.getOrElse(ByteStr.empty)

    def isLegacy: Boolean = tx.version == 1 || tx.version == 2

    def toVanillaOrAdapter: VanillaTransaction = if (this.isLegacy) toVanilla else toVanillaAdapter

    def toVanillaAdapter = new PBSignedTransactionVanillaAdapter(tx)

    def toVanilla: VanillaTransaction = tx.data match {
      case Data.Genesis(GenesisTransactionData(recipient, amount)) =>
        vt.GenesisTransaction(recipient.toAddress, amount, tx.timestamp, signature)

      case Data.Payment(PaymentTransactionData(recipient, amount)) =>
        vt.PaymentTransaction(tx.sender, recipient.toAddress, amount, tx.fee, tx.timestamp, signature)

      case Data.Transfer(TransferTransactionData(assetId, recipient, amount, attachment)) =>
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
                                     signature)
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

      case Data.CreateAlias(CreateAliasTransactionData(alias)) =>
        tx.version match {
          case 1 => vt.CreateAliasTransactionV1(tx.sender, alias.toAlias, tx.fee, tx.timestamp, signature)
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
              decimals.toByte,
              reissuable,
              tx.fee,
              tx.timestamp,
              signature
            )
          case 2 =>
            vt.assets.IssueTransactionV2(
              tx.chainId.byte,
              tx.sender,
              name,
              description,
              quantity,
              decimals.toByte,
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
          case 1 => vt.assets.ReissueTransactionV1(tx.sender, assetId, amount, reissuable, tx.fee, tx.timestamp, signature)
          case 2 => vt.assets.ReissueTransactionV2(tx.chainId, tx.sender, assetId, amount, reissuable, tx.fee, tx.timestamp, tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.Burn(BurnTransactionData(assetId, amount)) =>
        tx.version match {
          case 1 => vt.assets.BurnTransactionV1(tx.sender, assetId, amount, tx.fee, tx.timestamp, signature)
          case 2 => vt.assets.BurnTransactionV2(tx.chainId, tx.sender, assetId, amount, tx.fee, tx.timestamp, tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.SetAssetScript(SetAssetScriptTransactionData(assetId, script)) =>
        vt.assets.SetAssetScriptTransaction(tx.chainId, tx.sender, assetId, script, tx.fee, tx.timestamp, tx.proofs)

      case Data.SetScript(SetScriptTransactionData(script)) =>
        vt.smart.SetScriptTransaction(tx.chainId, tx.sender, script, tx.fee, tx.timestamp, tx.proofs)

      case Data.Lease(LeaseTransactionData(recipient, amount)) =>
        tx.version match {
          case 1 => vt.lease.LeaseTransactionV1(tx.sender, amount, tx.fee, tx.timestamp, recipient.toAddressOrAlias, signature)
          case 2 => vt.lease.LeaseTransactionV2(tx.sender, amount, tx.fee, tx.timestamp, recipient.toAddressOrAlias, tx.proofs)
          case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v")
        }

      case Data.LeaseCancel(LeaseCancelTransactionData(leaseId)) =>
        tx.version match {
          case 1 => vt.lease.LeaseCancelTransactionV1(tx.sender, leaseId, tx.fee, tx.timestamp, signature)
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
              signature
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
          transfers.map(t => ParsedTransfer(t.address.toAddressOrAlias, t.amount)).toList,
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

    def signed(signer: Array[Byte]): PBSignedTransaction = {
      import com.wavesplatform.common.utils._
      tx.withProofs(Proofs.create(Seq(ByteStr(crypto.sign(signer, tx.toVanillaAdapter.bodyBytes())))).explicitGet())
    }
  }
}

object PBSignedTransactionImplicits extends PBSignedTransactionImplicits
