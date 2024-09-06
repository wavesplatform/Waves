package com.wavesplatform.transaction

import com.google.protobuf.ByteString
import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.protobuf.transaction.{PBTransactions, SignedTransaction as PBSignedTransaction}
import com.wavesplatform.state.StringDataEntry
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction, Verifier}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import org.scalacheck.Gen

class ChainIdSpecification extends PropSpec {
  private val otherChainId   = 'W'.toByte
  private val aliasFromOther = Alias.createWithChainId("sasha", otherChainId, Some(otherChainId)).explicitGet()
  private val addressFromOther =
    Address.fromBytes(Base58.tryDecodeWithLimit("3P3oxTkpCWJgCr6SJrBzdP5N8jFqHCiy7L2").get, Some(otherChainId)).explicitGet()
  private val addressOrAlias = Gen.oneOf(aliasFromOther, addressFromOther)

  private def addressOrAliasWithVersion: Gen[(AddressOrAlias, TxVersion, KeyPair, TxPositiveAmount, TxPositiveAmount, TxTimestamp)] =
    for {
      addressOrAlias <- addressOrAlias
      version        <- Gen.oneOf(1, 2, 3)
      sender         <- accountGen
      amount         <- Gen.choose(1, 10000000L)
      fee            <- Gen.choose(1000000L, 10000000L)
      ts             <- Gen.choose(1, 1000000L)
    } yield (addressOrAlias, version.toByte, sender, TxPositiveAmount.unsafeFrom(amount), TxPositiveAmount.unsafeFrom(fee), ts)

  private def validateFromOtherNetwork(tx: Transaction): Unit = {
    tx.chainId should not be AddressScheme.current.chainId

    val protoTx       = PBTransactions.protobuf(tx)
    val recoveredTxEi = PBTransactions.vanilla(PBSignedTransaction.parseFrom(protoTx.toByteArray), unsafe = true)

    recoveredTxEi.explicitGet()

    val recoveredTx = recoveredTxEi.explicitGet().asInstanceOf[ProvenTransaction]

    recoveredTx shouldBe tx
    Verifier.verifyAsEllipticCurveSignature(recoveredTx, isRideV6Activated = false).explicitGet()
  }

  property("TransferTransaction validation") {
    forAll(addressOrAliasWithVersion) { case (addressOrAlias, version, sender, amount, fee, ts) =>
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
          Alias.createWithChainId("sasha", otherChainId, Some(otherChainId)).explicitGet(),
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
    forAll(addressOrAliasWithVersion) { case (_, _, sender, amount, fee, ts) =>
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
    forAll(addressOrAliasWithVersion) { case (addressOrAlias, version, sender, amount, fee, ts) =>
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
    forAll(addressOrAliasWithVersion) { case (addressOrAlias, version, sender, _, fee, ts) =>
      InvokeScriptTransaction(
        version,
        sender.publicKey,
        addressOrAlias,
        None,
        Seq.empty,
        fee,
        Waves,
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
          ts,
          Proofs.empty,
          otherChainId
        ).signWith(sender.privateKey).validatedEither.explicitGet()
      )
    }
  }

  property("GenesisTransaction validation") {
    forAll(addressOrAliasWithVersion) { case (_, _, _, amount, _, ts) =>
      GenesisTransaction(
        addressFromOther,
        TxNonNegativeAmount.unsafeFrom(amount.value),
        ts,
        ByteStr.empty,
        AddressScheme.current.chainId
      ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))
    }
  }

  property("BurnTransaction validation") {
    forAll(addressOrAliasWithVersion) { case (_, _, sender, amount, fee, ts) =>
      validateFromOtherNetwork(
        BurnTransaction(
          TxVersion.V3,
          sender.publicKey,
          IssuedAsset(ByteStr(bytes32gen.sample.get)),
          TxNonNegativeAmount.unsafeFrom(amount.value),
          fee,
          ts,
          Proofs.empty,
          otherChainId
        ).signWith(sender.privateKey).validatedEither.explicitGet()
      )
    }
  }

  property("CreateAliasTransaction validation") {
    forAll(addressOrAliasWithVersion) { case (_, _, sender, _, fee, ts) =>
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
    forAll(addressOrAliasWithVersion) { case (_, _, sender, _, fee, ts) =>
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
    forAll(addressOrAliasWithVersion) { case (_, _, sender, amount, fee, ts) =>
      val pair = AssetPair(Waves, IssuedAsset(ByteStr(bytes32gen.sample.get)))
      validateFromOtherNetwork(
        ExchangeTransaction(
          TxVersion.V3,
          Order.sell(Order.V3, sender, sender.publicKey, pair, amount.value, amount.value, ts, ts + ts, fee.value).explicitGet(),
          Order.buy(Order.V3, sender, sender.publicKey, pair, amount.value, amount.value, ts, ts + ts, fee.value).explicitGet(),
          TxExchangeAmount.unsafeFrom(amount.value),
          TxExchangePrice.unsafeFrom(amount.value),
          fee.value,
          fee.value,
          fee,
          ts,
          Proofs.empty,
          otherChainId
        ).signWith(sender.privateKey).validatedEither.explicitGet()
      )
    }
  }

  property("IssueTransaction validation") {
    forAll(addressOrAliasWithVersion) { case (_, _, sender, quantity, fee, ts) =>
      validateFromOtherNetwork(
        IssueTransaction(
          TxVersion.V3,
          sender.publicKey,
          ByteString.copyFromUtf8("name"),
          ByteString.copyFromUtf8("description"),
          quantity,
          TxDecimals.unsafeFrom(8: Byte),
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
    forAll(addressOrAliasWithVersion) { case (_, _, sender, _, fee, ts) =>
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
    forAll(addressOrAliasWithVersion) { case (addressOrAlias, _, sender, amount, fee, ts) =>
      validateFromOtherNetwork(
        MassTransferTransaction(
          TxVersion.V2,
          sender.publicKey,
          Waves,
          Seq(ParsedTransfer(addressOrAlias, TxNonNegativeAmount.unsafeFrom(amount.value))),
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
    forAll(addressOrAliasWithVersion) { case (_, _, sender, quantity, fee, ts) =>
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
    forAll(addressOrAliasWithVersion) { case (_, _, sender, _, fee, ts) =>
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
    forAll(addressOrAliasWithVersion) { case (_, _, sender, _, fee, ts) =>
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
    forAll(addressOrAliasWithVersion) { case (_, _, sender, _, fee, ts) =>
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
    forAll(addressOrAliasWithVersion) { case (_, version, sender, _, fee, ts) =>
      validateFromOtherNetwork(
        UpdateAssetInfoTransaction(
          version,
          sender.publicKey,
          IssuedAsset(ByteStr(bytes32gen.sample.get)),
          "name",
          "description",
          ts,
          fee,
          Waves,
          Proofs.empty,
          otherChainId
        ).signWith(sender.privateKey).validatedEither.explicitGet()
      )
    }
  }
}
