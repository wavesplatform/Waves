package com.wavesplatform.transaction

import cats.data.NonEmptyList
import com.wavesplatform.account.Alias
import com.wavesplatform.common.state.diffs.ProduceError
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.DomainScenarioDrivenPropertyCheck
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.{ENOUGH_AMT, FeeValidation}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class TransactionFeesSpec
    extends PropSpec
    with ScalaCheckDrivenPropertyChecks
    with DomainScenarioDrivenPropertyCheck
    with Matchers
    with TransactionGen
    with WithDomain
    with NoShrink {

  object Setup {
    val preconditions = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    } yield (genesis, master)

    val assetName = "Shitcoib".getBytes
    val alias     = Alias("putin")

    val invalidNftIssue = for {
      (genesis, master) <- preconditions
      issue             <- createIssue(master, assetName, assetName, 1, 0, reissuable = false, (FeeValidation.FeeUnit * 0.9).toLong, genesis.timestamp + 1)
    } yield (master, NonEmptyList.of(genesis, issue))

    val validNftIssue = for {
      (genesis, master) <- preconditions
      issue             <- createIssue(master, assetName, assetName, 1, 0, reissuable = false, FeeValidation.FeeUnit, genesis.timestamp + 1)
    } yield (master, NonEmptyList.of(genesis, issue))

    val invalidIssue = for {
      (genesis, master) <- preconditions
      issue <- createIssue(
        master,
        assetName,
        assetName,
        1,
        8,
        reissuable = false,
        (FeeValidation.FeeUnit * 10000 * 0.9).toLong,
        genesis.timestamp + 1
      )
    } yield (master, NonEmptyList.of(genesis, issue))

    val validIssue = for {
      (genesis, master) <- preconditions
      issue             <- createIssue(master, assetName, assetName, 1, 8, reissuable = false, FeeValidation.FeeUnit * 100000, genesis.timestamp + 1)
    } yield (master, NonEmptyList.of(genesis, issue))

    val invalidReissue = for {
      (genesis, master) <- preconditions
      issue             <- createIssue(master, assetName, assetName, 1, 8, reissuable = true, FeeValidation.FeeUnit * 100000, genesis.timestamp + 1)
      reissue           <- createReissue(master, IssuedAsset(issue.assetId), 1, reissuable = true, (FeeValidation.FeeUnit * 0.9).toLong, genesis.timestamp + 2)
    } yield (master, NonEmptyList.of(genesis, issue, reissue))

    val validReissue = for {
      (genesis, master) <- preconditions
      issue             <- createIssue(master, assetName, assetName, 1, 8, reissuable = true, FeeValidation.FeeUnit * 100000, genesis.timestamp + 1)
      reissue           <- createReissue(master, IssuedAsset(issue.assetId), 1, reissuable = true, FeeValidation.FeeUnit, genesis.timestamp + 2)
    } yield (master, NonEmptyList.of(genesis, issue, reissue))

    val invalidAlias = for {
      (genesis, master) <- preconditions
      alias             <- createAliasGen(master, alias, (FeeValidation.FeeUnit * 1000 * 0.9).toLong, genesis.timestamp + 1)
    } yield (master, NonEmptyList.of(genesis, alias))

    val validAlias = for {
      (genesis, master) <- preconditions
      alias             <- createAliasGen(master, alias, FeeValidation.FeeUnit * 1000, genesis.timestamp + 1)
    } yield (master, NonEmptyList.of(genesis, alias))
  }

  val (valid, invalid) = {
    import Setup._
    val valid = Seq(
      validNftIssue :| "valid nft issue",
      validIssue :| "valid issue",
      validReissue :| "valid reissue",
      validAlias :| "valid alias"
    )

    val invalid = Seq(
      invalidNftIssue :| "invalid nft issue",
      invalidIssue :| "invalid issue",
      invalidReissue :| "invalid reissue",
      invalidAlias :| "invalid alias"
    )

    (valid, invalid)
  }

  val functionalitySettings = TestFunctionalitySettings.withFeatures(BlockchainFeatures.IncreaseIssueFee, BlockchainFeatures.ReduceNFTFee, BlockchainFeatures.FeeSponsorship)
    .copy(featureCheckBlocksPeriod = 1, blocksForFeatureActivation = 1)

  property("transactions with sufficient fee processed") {
    valid.foreach(forAll(_) {
      case (signer, NonEmptyList(genesis, rest)) =>
        withDomainFS(functionalitySettings) { d =>
          val gb = TestBlock.create(Seq(genesis))
          d.blockchainUpdater.processBlock(gb) shouldBe 'right
          d.blockchainUpdater.processBlock(TestBlock.create(signer, rest, gb.uniqueId)) shouldBe 'right
        }
    })
  }

  property("transactions with insufficient fee dropped") {
    invalid.foreach(forAll(_) {
      case (signer, NonEmptyList(genesisTx, rest)) =>
        withDomainFS(functionalitySettings) { d =>
          val gb = TestBlock.create(Seq(genesisTx))
          d.blockchainUpdater.processBlock(gb) shouldBe 'right
          d.blockchainUpdater.processBlock(TestBlock.create(signer, rest, gb.uniqueId)) should ProduceError.produce("does not exceed minimal value")
        }
    })
  }
}
