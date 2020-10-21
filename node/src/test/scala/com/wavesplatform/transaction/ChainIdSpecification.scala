package com.wavesplatform.transaction

import com.google.protobuf.ByteString
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.protobuf.transaction.{PBTransactions, SignedTransaction => PBSignedTransaction}
import com.wavesplatform.state.StringDataEntry
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction, Verifier}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.{TransactionGen, crypto}
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ChainIdSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  private val otherChainId     = 'W'.toByte
  private val aliasFromOther   = Alias.createWithChainId("sasha", otherChainId).explicitGet()
  private val addressFromOther = Address.fromBytes(Base58.tryDecodeWithLimit("3P3oxTkpCWJgCr6SJrBzdP5N8jFqHCiy7L2").get, otherChainId).explicitGet()
  private val addressOrAlias   = Gen.oneOf(aliasFromOther, addressFromOther)

  private def addressOrAliasWithVersion(vs: Set[TxVersion]): Gen[(AddressOrAlias, TxVersion, KeyPair, TxAmount, TxAmount, TxTimestamp)] =
    for {
      addressOrAlias <- addressOrAlias
      version        <- Gen.oneOf(vs.toSeq)
      sender         <- accountGen
      amount         <- Gen.choose(1, 10000000L)
      fee            <- Gen.choose(1000000L, 10000000L)
      ts             <- Gen.choose(1, 1000000L)
    } yield (addressOrAlias, version, sender, amount, fee, ts)

  private def validateFromOtherNetwork(tx: Transaction): Unit = {
    tx.chainId should not be AddressScheme.current.chainId

    val protoTx       = PBTransactions.protobuf(tx)
    val recoveredTxEi = PBTransactions.vanilla(PBSignedTransaction.parseFrom(protoTx.toByteArray))

    recoveredTxEi.explicitGet()

    val recoveredTx = recoveredTxEi.explicitGet().asInstanceOf[ProvenTransaction]

    recoveredTx shouldBe tx
    Verifier.verifyAsEllipticCurveSignature(recoveredTx).explicitGet()
  }

  property("TransferTransaction validation") {
    forAll(addressOrAliasWithVersion(TransferTransaction.supportedVersions)) {
      case (addressOrAlias, version, sender, amount, fee, ts) =>
        TransferTransaction(
          version,
          sender.publicKey,
          addressOrAlias,
          Waves,
          amount,
          Waves,
          fee,
          ByteStr.empty,
          ts,
          Proofs.empty,
          AddressScheme.current.chainId
        ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))

        validateFromOtherNetwork(
          TransferTransaction(
            TxVersion.V3,
            sender.publicKey,
            Alias.createWithChainId("sasha", otherChainId).explicitGet(),
            Waves,
            amount,
            Waves,
            fee,
            ByteStr.empty,
            ts,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }

  property("PaymentTransaction validation") {
    forAll(addressOrAliasWithVersion(PaymentTransaction.supportedVersions)) {
      case (_, _, sender, amount, fee, ts) =>
        PaymentTransaction(
          sender.publicKey,
          addressFromOther,
          amount,
          fee,
          ts,
          ByteStr.empty,
          AddressScheme.current.chainId
        ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))

        validateFromOtherNetwork(
          PaymentTransaction(
            sender.publicKey,
            addressFromOther,
            amount,
            fee,
            ts,
            ByteStr.empty,
            otherChainId
          ).validatedEither.map(u => u.copy(signature = crypto.sign(sender.privateKey, u.bodyBytes()))).explicitGet()
        )
    }
  }

  property("LeaseTransaction validation") {
    forAll(addressOrAliasWithVersion(LeaseTransaction.supportedVersions)) {
      case (addressOrAlias, version, sender, amount, fee, ts) =>
        LeaseTransaction(
          version,
          sender.publicKey,
          addressOrAlias,
          amount,
          fee,
          ts,
          Proofs.empty,
          AddressScheme.current.chainId
        ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))

        validateFromOtherNetwork(
          LeaseTransaction(
            TxVersion.V3,
            sender.publicKey,
            addressOrAlias,
            amount,
            fee,
            ts,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }

  property("InvokeScriptTransaction validation") {
    forAll(addressOrAliasWithVersion(InvokeScriptTransaction.supportedVersions)) {
      case (addressOrAlias, version, sender, _, fee, ts) =>
        InvokeScriptTransaction(
          version,
          sender.publicKey,
          addressOrAlias,
          None,
          Seq.empty,
          fee,
          Waves,
          extraFeePerStep = InvokeScriptTransaction.DefaultExtraFeePerStep,
          ts,
          Proofs.empty,
          AddressScheme.current.chainId
        ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))

        validateFromOtherNetwork(
          InvokeScriptTransaction(
            TxVersion.V2,
            sender.publicKey,
            addressOrAlias,
            None,
            Seq.empty,
            fee,
            Waves,
            extraFeePerStep = InvokeScriptTransaction.DefaultExtraFeePerStep,
            ts,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }

  property("GenesisTransaction validation") {
    forAll(addressOrAliasWithVersion(GenesisTransaction.supportedVersions)) {
      case (_, _, _, amount, _, ts) =>
        GenesisTransaction(
          addressFromOther,
          amount,
          ts,
          ByteStr.empty,
          AddressScheme.current.chainId
        ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))
    }
  }

  property("BurnTransaction validation") {
    forAll(addressOrAliasWithVersion(BurnTransaction.supportedVersions)) {
      case (_, _, sender, amount, fee, ts) =>
        validateFromOtherNetwork(
          BurnTransaction(
            TxVersion.V3,
            sender.publicKey,
            IssuedAsset(ByteStr(bytes32gen.sample.get)),
            amount,
            fee,
            ts,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }

  property("CreateAliasTransaction validation") {
    forAll(addressOrAliasWithVersion(CreateAliasTransaction.supportedVersions)) {
      case (_, _, sender, _, fee, ts) =>
        validateFromOtherNetwork(
          CreateAliasTransaction(
            TxVersion.V3,
            sender.publicKey,
            "alias",
            fee,
            ts,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }

  property("DataTransaction validation") {
    forAll(addressOrAliasWithVersion(DataTransaction.supportedVersions)) {
      case (_, _, sender, _, fee, ts) =>
        validateFromOtherNetwork(
          DataTransaction(
            TxVersion.V2,
            sender.publicKey,
            Seq(StringDataEntry("key", "value")),
            fee,
            ts,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }

  property("ExchangeTransaction validation") {
    forAll(addressOrAliasWithVersion(ExchangeTransaction.supportedVersions)) {
      case (_, _, sender, amount, fee, ts) =>
        val pair = AssetPair(Waves, IssuedAsset(ByteStr(bytes32gen.sample.get)))
        validateFromOtherNetwork(
          ExchangeTransaction(
            TxVersion.V3,
            Order.sell(Order.V3, sender, sender.publicKey, pair, amount, amount, ts, ts + ts, fee),
            Order.buy(Order.V3, sender, sender.publicKey, pair, amount, amount, ts, ts + ts, fee),
            amount,
            amount,
            fee,
            fee,
            fee,
            ts,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }

  property("IssueTransaction validation") {
    forAll(addressOrAliasWithVersion(IssueTransaction.supportedVersions)) {
      case (_, _, sender, quantity, fee, ts) =>
        validateFromOtherNetwork(
          IssueTransaction(
            TxVersion.V3,
            sender.publicKey,
            ByteString.copyFromUtf8("name"),
            ByteString.copyFromUtf8("description"),
            quantity,
            8: Byte,
            true,
            None,
            fee,
            ts,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }

  property("LeaseCancelTransaction validation") {
    forAll(addressOrAliasWithVersion(LeaseCancelTransaction.supportedVersions)) {
      case (_, _, sender, _, fee, ts) =>
        validateFromOtherNetwork(
          LeaseCancelTransaction(
            TxVersion.V3,
            sender.publicKey,
            ByteStr(bytes32gen.sample.get),
            fee,
            ts,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }

  property("MassTransferTransaction validation") {
    forAll(addressOrAliasWithVersion(MassTransferTransaction.supportedVersions)) {
      case (addressOrAlias, _, sender, amount, fee, ts) =>
        validateFromOtherNetwork(
          MassTransferTransaction(
            TxVersion.V2,
            sender.publicKey,
            Waves,
            Seq(ParsedTransfer(addressOrAlias, amount)),
            fee,
            ts,
            ByteStr.empty,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }

  property("ReissueTransaction validation") {
    forAll(addressOrAliasWithVersion(ReissueTransaction.supportedVersions)) {
      case (_, _, sender, quantity, fee, ts) =>
        validateFromOtherNetwork(
          ReissueTransaction(
            TxVersion.V3,
            sender.publicKey,
            IssuedAsset(ByteStr(bytes32gen.sample.get)),
            quantity,
            true,
            fee,
            ts,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }

  property("SetAssetScriptTransaction validation") {
    forAll(addressOrAliasWithVersion(SetAssetScriptTransaction.supportedVersions)) {
      case (_, _, sender, _, fee, ts) =>
        validateFromOtherNetwork(
          SetAssetScriptTransaction(
            TxVersion.V2,
            sender.publicKey,
            IssuedAsset(ByteStr(bytes32gen.sample.get)),
            Some(scriptGen.sample.get),
            fee,
            ts,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }

  property("SetScriptTransaction validation") {
    forAll(addressOrAliasWithVersion(SetScriptTransaction.supportedVersions)) {
      case (_, _, sender, _, fee, ts) =>
        validateFromOtherNetwork(
          SetScriptTransaction(
            TxVersion.V2,
            sender.publicKey,
            Some(scriptGen.sample.get),
            fee,
            ts,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }

  property("SponsorFeeTransaction validation") {
    forAll(addressOrAliasWithVersion(SponsorFeeTransaction.supportedVersions)) {
      case (_, _, sender, _, fee, ts) =>
        validateFromOtherNetwork(
          SponsorFeeTransaction(
            TxVersion.V2,
            sender.publicKey,
            IssuedAsset(ByteStr(bytes32gen.sample.get)),
            None,
            fee,
            ts,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }

  property("UpdateAssetInfoTransaction validation") {
    forAll(addressOrAliasWithVersion(UpdateAssetInfoTransaction.supportedVersions)) {
      case (_, version, sender, _, fee, ts) =>
        validateFromOtherNetwork(
          UpdateAssetInfoTransaction(
            version,
            sender.publicKey,
            IssuedAsset(ByteStr(bytes32gen.sample.get)),
            "name",
            "description",
            fee,
            ts,
            Waves,
            Proofs.empty,
            otherChainId
          ).signWith(sender.privateKey).validatedEither.explicitGet()
        )
    }
  }
}
