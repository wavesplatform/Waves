package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.{Address, AddressScheme, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ChainIdSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {
  property("TransferTransaction recipient validation") {
    forAll(Gen.oneOf(TransferTransaction.supportedVersions.toSeq)) { v =>
      TransferTransaction(
        v,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Alias.createWithChainId("sasha", 'W'.toByte).explicitGet(),
        Waves,
        100000000,
        Waves,
        100000000,
        None,
        1526641218066L,
        Proofs.empty,
        AddressScheme.current.chainId
      ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))

      TransferTransaction(
        v,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Address.fromBytes(Base58.tryDecodeWithLimit("3P3oxTkpCWJgCr6SJrBzdP5N8jFqHCiy7L2").get, chainId = 'W'.toByte).explicitGet(),
        Waves,
        100000000,
        Waves,
        100000000,
        None,
        1526641218066L,
        Proofs.empty,
        AddressScheme.current.chainId
      ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))
    }
  }

  property("PaymentTransaction recipient validation") {
    PaymentTransaction(
      PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
      Address.fromBytes(Base58.tryDecodeWithLimit("3P3oxTkpCWJgCr6SJrBzdP5N8jFqHCiy7L2").get, chainId = 'W'.toByte).explicitGet(),
      100000000,
      100000000,
      1526641218066L,
      ByteStr.empty,
      AddressScheme.current.chainId
    ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))
  }

  property("CreateAliasTransaction alias validation") {
    forAll(Gen.oneOf(TransferTransaction.supportedVersions.toSeq)) { v =>
      CreateAliasTransaction(
        v,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Alias.createWithChainId("sasha", 'W'.toByte).explicitGet(),
        100000000,
        1526641218066L,
        Proofs.empty,
        AddressScheme.current.chainId
      ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))
    }
  }

  property("LeaseTransaction recipient validation") {
    forAll(Gen.oneOf(TransferTransaction.supportedVersions.toSeq)) { v =>
      LeaseTransaction(
        v,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Alias.createWithChainId("sasha", 'W'.toByte).explicitGet(),
        100000000,
        100000000,
        1526641218066L,
        Proofs.empty,
        AddressScheme.current.chainId
      ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))

      LeaseTransaction(
        v,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Address.fromBytes(Base58.tryDecodeWithLimit("3P3oxTkpCWJgCr6SJrBzdP5N8jFqHCiy7L2").get, chainId = 'W'.toByte).explicitGet(),
        100000000,
        100000000,
        1526641218066L,
        Proofs.empty,
        AddressScheme.current.chainId
      ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))
    }
  }

  property("InvokeScriptTransaction dAppAddressOrAlias validation") {
    forAll(Gen.oneOf(TransferTransaction.supportedVersions.toSeq)) { v =>
      InvokeScriptTransaction(
        v,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Alias.createWithChainId("sasha", 'W'.toByte).explicitGet(),
        None,
        Seq.empty,
        100000000,
        Waves,
        1526641218066L,
        Proofs.empty,
        AddressScheme.current.chainId
      ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))

      InvokeScriptTransaction(
        v,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Address.fromBytes(Base58.tryDecodeWithLimit("3P3oxTkpCWJgCr6SJrBzdP5N8jFqHCiy7L2").get, chainId = 'W'.toByte).explicitGet(),
        None,
        Seq.empty,
        100000000,
        Waves,
        1526641218066L,
        Proofs.empty,
        AddressScheme.current.chainId
      ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))
    }
  }

  property("GenesisTransaction recipient validation") {
    GenesisTransaction(
      Address.fromBytes(Base58.tryDecodeWithLimit("3P3oxTkpCWJgCr6SJrBzdP5N8jFqHCiy7L2").get, chainId = 'W'.toByte).explicitGet(),
      100000000,
      1526641218066L,
      ByteStr.empty,
      AddressScheme.current.chainId
    ).validatedEither shouldBe Left(GenericError("Address or alias from other network"))
  }
}
