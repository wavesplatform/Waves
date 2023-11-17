package com.wavesplatform.transaction

import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state.diffs._
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.{IssueTransaction, SponsorFeeTransaction}
import com.wavesplatform.transaction.serialization.impl.SponsorFeeTxSerializer
import com.wavesplatform.transaction.transfer.TransferTransaction
import org.scalacheck.Gen
import play.api.libs.json.Json

class SponsorFeeTransactionSpecification extends PropSpec with WithState {
  val One = 100000000L
  val NgAndSponsorshipSettings: FunctionalitySettings = TestFunctionalitySettings.Enabled.copy(featureCheckBlocksPeriod = 1, blocksForFeatureActivation = 1, preActivatedFeatures = Map(NG.id -> 0, FeeSponsorship.id -> 0, SmartAccounts.id -> 0))
  val BlockV5Settings: FunctionalitySettings = NgAndSponsorshipSettings.copy(preActivatedFeatures = NgAndSponsorshipSettings.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0))

  property("SponsorFee serialization roundtrip") {
    forAll(sponsorFeeGen) { (tx: SponsorFeeTransaction) =>
      val recovered = SponsorFeeTransaction.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("decode pre-encoded bytes") {
    val bytes = Base64.decode(
      "AA4BDgG2DPWVCbVaxm9js3LYdZnhlWTRzVqNW4nurEvoDdnFLfweiKVqJfyZOK39MkvNISLB/ylUNT0ycoPSLGCPR6oaAAAAAAJIpUEAAAAABfXhAAAADF1swIRMAQABAEAlGGbLsMhr+34lYt3/Tx7XT76Al4D/V5xOhHwntdW2jR+/1XA6ku20SU6tPHphxo2+wFOxyJcPWEOBptAuw1oL"
    )
    val json = Json.parse("""
        |{
        |  "senderPublicKey" : "DFefQsRMtXtTKtpVBwsrD3mPAzerWxLXWHPEe9ANc548",
        |  "sender" : "3N3dKf1VfhfF6QuxeyBcKL73czXE6nys27u",
        |  "feeAssetId" : null,
        |  "proofs" : [ "k1ve9smBuVvEiRHGjVSpBwtUfPG4yETrxpXWPGEu83ddo5DdycGcEY4qfav8A1Ej9reCEdEivwRpWZ72zZ12X54" ],
        |  "assetId" : "HyAkA27DbuhLcFmimoSXoD9H5FP99JX5PcSXvMni4UWM",
        |  "fee" : 100000000,
        |  "minSponsoredAssetFee" : 38315329,
        |  "id" : "QhCGqFtJncL8y6eAgyGFP4xQBoXbBH4uaB3iKaRZLy8",
        |  "type" : 14,
        |  "version" : 1,
        |  "timestamp" : 13595396047948
        |}
        |""".stripMargin)

    val tx = SponsorFeeTxSerializer.parseBytes(bytes).get
    tx.json() shouldBe json
    assert(crypto.verify(tx.signature, tx.bodyBytes(), tx.sender), "signature should be valid")
  }

  property("SponsorFee serialization from TypedTransaction") {
    forAll(sponsorFeeGen) { (transaction: SponsorFeeTransaction) =>
      val recovered = TransactionParsers.parseBytes(transaction.bytes()).get
      recovered.bytes() shouldEqual transaction.bytes()
    }
  }

  property("JSON format validation") {
    val js = Json.parse(s"""{
 "type": 14,
 "id": "Gobt7AiyQAfduRkW8Mk3naWbzH67Zsv9rdmgRNmon1Mb",
 "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
 "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
 "fee": $One,
 "feeAssetId": null,
 "timestamp": 1520945679531,
 "proofs": [
 "3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"
 ],
 "version": 1,
 "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
 "minSponsoredAssetFee": 100000
                       }
    """)

    val tx = SponsorFeeTransaction
      .create(
        1.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get),
        Some(100000L),
        One,
        1520945679531L,
        Proofs(Seq(ByteStr.decodeBase58("3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7").get))
      )
      .explicitGet()
    js shouldEqual tx.json()
  }

  property("JSON format validation for canceling sponsorship") {
    val js = Json.parse(s"""{
 "type": 14,
 "id": "HsEHzgJEkmjy2aenmbNVFrK1Na9rz4V4p7o2fY9eB9za",
 "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
 "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
 "fee": $One,
 "feeAssetId":null,
 "timestamp": 1520945679531,
 "proofs": [
 "3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7"
 ],
 "version": 1,
 "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
 "minSponsoredAssetFee": null
                       }
    """)

    val tx = SponsorFeeTransaction
      .create(
        1.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get),
        None,
        One,
        1520945679531L,
        Proofs(Seq(ByteStr.decodeBase58("3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7").get))
      )
      .explicitGet()
    val tx1 = SponsorFeeTransaction
      .create(
        1.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get),
        None,
        One,
        1520945679531L,
        Proofs(Seq(ByteStr.decodeBase58("3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7").get))
      )
      .explicitGet()
    js shouldEqual tx.json()
    js shouldEqual tx1.json()
  }
  private val invalidFee =
    Table(
      "fee",
      -1 * Constants.UnitsInWave,
      0
    )

  property("sponsorship negative fee") {
    forAll(invalidFee) { (fee: Long) =>
      for {
        sender                                                                       <- accountGen
        (_, assetName, description, quantity, decimals, reissuable, iFee, timestamp) <- issueParamGen
        issue = IssueTransaction
          .selfSigned(
            TxVersion.V1,
            sender,
            new String(assetName),
            new String(description),
            quantity,
            decimals,
            reissuable = reissuable,
            script = None,
            iFee,
            timestamp
          )
          .explicitGet()
        minFee <- smallFeeGen
        assetId = issue.assetId
      } yield SponsorFeeTransaction.selfSigned(1.toByte, sender, IssuedAsset(assetId), Some(minFee), fee, timestamp) should produce(
        "insufficient fee"
      )
    }
  }

  property("cancel sponsorship negative fee") {
    forAll(invalidFee) { (fee: Long) =>
      for {
        sender                                                                       <- accountGen
        (_, assetName, description, quantity, decimals, reissuable, iFee, timestamp) <- issueParamGen
        issue = IssueTransaction
          .selfSigned(
            TxVersion.V1,
            sender,
            new String(assetName),
            new String(description),
            quantity,
            decimals,
            reissuable = reissuable,
            script = None,
            iFee,
            timestamp
          )
          .explicitGet()
        minFee  = None
        assetId = issue.assetId
      } yield SponsorFeeTransaction.selfSigned(1.toByte, sender, IssuedAsset(assetId), minFee, fee, timestamp) should produce("insufficient fee")
    }
  }

  property("miner receives one satoshi less than sponsor pays") {
    val setup = for {
      (acc, name, desc, quantity, decimals, reissuable, fee, ts) <- issueParamGen
      genesis = GenesisTransaction.create(acc.toAddress, ENOUGH_AMT, ts).explicitGet()
      issue = IssueTransaction
        .selfSigned(TxVersion.V1, acc, new String(name), new String(desc), quantity, decimals, reissuable, script = None, fee, ts)
        .explicitGet()
      minFee <- Gen.choose(1L, issue.quantity.value)
      sponsor = SponsorFeeTransaction.selfSigned(1.toByte, acc, IssuedAsset(issue.id()), Some(minFee), One, ts).explicitGet()
      transfer = TransferTransaction
        .selfSigned(1.toByte, acc, acc.toAddress, Waves, 1L, feeAsset = IssuedAsset(issue.id()), minFee, ByteStr.empty, ts)
        .explicitGet()
    } yield (acc, genesis, issue, sponsor, transfer)

    forAll(setup) {
      case (acc, genesis, issue, sponsor, transfer) =>
        val b0 = block(acc, Seq(genesis, issue, sponsor))
        val b1 = block(acc, Seq(transfer))
        val b2 = block(acc, Seq.empty)

        assertNgDiffState(Seq(b0, b1), b2, NgAndSponsorshipSettings) {
          case (_, state) =>
            state.balance(acc.toAddress, Waves) - ENOUGH_AMT shouldBe 0
        }
    }
  }

  property("miner receives one satoshi more than sponsor pays") {
    val setup = for {
      (acc, name, desc, quantity, decimals, reissuable, fee, ts) <- issueParamGen
      genesis = GenesisTransaction.create(acc.toAddress, ENOUGH_AMT, ts).explicitGet()
      issue = IssueTransaction
        .selfSigned(TxVersion.V1, acc, new String(name), new String(desc), quantity, decimals, reissuable, script = None, fee, ts)
        .explicitGet()
      minFee <- Gen.choose(1000000L, issue.quantity.value)
      sponsor = SponsorFeeTransaction.selfSigned(1.toByte, acc, IssuedAsset(issue.id()), Some(minFee), One, ts).explicitGet()
      transfer1 = TransferTransaction
        .selfSigned(1.toByte, acc, acc.toAddress, Waves, 1L, feeAsset = IssuedAsset(issue.id()), minFee + 7, ByteStr.empty, ts)
        .explicitGet()
      transfer2 = TransferTransaction
        .selfSigned(1.toByte, acc, acc.toAddress, Waves, 1L, feeAsset = IssuedAsset(issue.id()), minFee + 9, ByteStr.empty, ts)
        .explicitGet()
    } yield (acc, genesis, issue, sponsor, transfer1, transfer2)

    forAll(setup) {
      case (acc, genesis, issue, sponsor, transfer1, transfer2) =>
        val b0 = block(acc, Seq(genesis, issue, sponsor))
        val b1 = block(acc, Seq(transfer1, transfer2))
        val b2 = block(acc, Seq.empty)

        assertNgDiffState(Seq(b0, b1), b2, NgAndSponsorshipSettings) {
          case (_, state) =>
            state.balance(acc.toAddress, Waves) - ENOUGH_AMT shouldBe 0
        }
    }
  }

  property("sponsorship changes in the middle of a block") {
    val setup = for {
      (acc, name, desc, quantity, decimals, reissuable, fee, ts) <- issueParamGen
      genesis = GenesisTransaction.create(acc.toAddress, ENOUGH_AMT, ts).explicitGet()
      issue = IssueTransaction
        .selfSigned(TxVersion.V1, acc, new String(name), new String(desc), quantity, decimals, reissuable, script = None, fee, ts)
        .explicitGet()
      minFee <- Gen.choose(1, issue.quantity.value / 11)

      sponsor1 = SponsorFeeTransaction.selfSigned(1.toByte, acc, IssuedAsset(issue.id()), Some(minFee), One, ts).explicitGet()
      transfer1 = TransferTransaction
        .selfSigned(1.toByte, acc, acc.toAddress, Waves, 1L, IssuedAsset(issue.id()), fee = minFee, ByteStr.empty, ts)
        .explicitGet()
      sponsor2 = SponsorFeeTransaction.selfSigned(1.toByte, acc, IssuedAsset(issue.id()), Some(minFee * 10), One, ts).explicitGet()
      transfer2 = TransferTransaction
        .selfSigned(1.toByte, acc, acc.toAddress, Waves, 1L, IssuedAsset(issue.id()), fee = minFee * 10, ByteStr.empty, ts)
        .explicitGet()
    } yield (acc, genesis, issue, sponsor1, transfer1, sponsor2, transfer2)

    forAll(setup) {
      case (acc, genesis, issue, sponsor1, transfer1, sponsor2, transfer2) =>
        val b0 = block(acc, Seq(genesis, issue, sponsor1))
        val b1 = block(acc, Seq(transfer1, sponsor2, transfer2))
        val b2 = block(acc, Seq.empty)

        assertNgDiffState(Seq(b0, b1), b2, NgAndSponsorshipSettings) {
          case (_, state) =>
            state.balance(acc.toAddress, Waves) - ENOUGH_AMT shouldBe 0
        }
    }
  }

  property(s"min fee changed after ${BlockchainFeatures.BlockV5} activation") {
    val setup = for {
      (acc, name, desc, quantity, decimals, reissuable, fee, ts) <- issueParamGen
      genesis = GenesisTransaction.create(acc.toAddress, ENOUGH_AMT, ts).explicitGet()
      issue = IssueTransaction
        .selfSigned(TxVersion.V1, acc, new String(name), new String(desc), quantity, decimals, reissuable, script = None, fee, ts)
        .explicitGet()
      minSponsoredAssetFee <- Gen.choose(1, issue.quantity.value / 11)
      minFee               <- Gen.choose(One / 1000, One - 1)
      sponsor = SponsorFeeTransaction.selfSigned(1.toByte, acc, IssuedAsset(issue.id()), Some(minSponsoredAssetFee), minFee, ts).explicitGet()
    } yield (genesis, issue, sponsor, minFee)

    forAll(setup) {
      case (genesis, issue, sponsor, actualFee) =>
        val b0 = block(Seq(genesis, issue))
        val b1 = block(Seq(sponsor))
        val b2 = block(Seq(sponsor), Block.ProtoBlockVersion)

        assertDiffEi(Seq(b0), b1, NgAndSponsorshipSettings) { ei =>
          ei should produce(s"Fee for SponsorFeeTransaction ($actualFee in WAVES) does not exceed minimal value of $One WAVES.")
        }

        assertDiffEi(Seq(b0), b2, BlockV5Settings) { ei =>
          ei.explicitGet()
        }
    }
  }
}
