package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.{Alias, KeyPair}
import com.wavesplatform.common.state.diffs.ProduceError
import com.wavesplatform.common.utils._
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.DomainScenarioDrivenPropertyCheck
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.{ENOUGH_AMT, FeeValidation}
import com.wavesplatform.transaction.Asset.IssuedAsset
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class TransactionFeesSpec
    extends PropSpec
    with ScalaCheckDrivenPropertyChecks
    with DomainScenarioDrivenPropertyCheck
    with Matchers
    with TransactionGen
with WithDomain {

  type Setup = (GenesisTransaction, KeyPair)
  object Setup {
    val preconditions: Gen[Setup] = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    } yield (genesis, master)

    val assetName = "123".getBytes

    val invalidNftIssue = for {
      (genesis, master) <- preconditions
      issue             <- createIssue(master, assetName, assetName, 1, 1, reissuable = false, (FeeValidation.FeeUnit * 0.9).toLong, genesis.timestamp + 1)
    } yield (master, Seq(genesis, issue))

    val validNftIssue = for {
      (genesis, master) <- preconditions
      issue             <- createIssue(master, assetName, assetName, 1, 1, reissuable = false, FeeValidation.FeeUnit, genesis.timestamp + 1)
    } yield (master, Seq(genesis, issue))

    val invalidIssue = for {
      (genesis, master) <- preconditions
      issue <- createIssue(
        master,
        assetName,
        assetName,
        1,
        8,
        reissuable = false,
        (FeeValidation.FeeUnit * 100000 * 0.9).toLong,
        genesis.timestamp + 1
      )
    } yield (master, Seq(genesis, issue))

    val validIssue = for {
      (genesis, master) <- preconditions
      issue             <- createIssue(master, assetName, assetName, 1, 8, reissuable = false, FeeValidation.FeeUnit * 100000, genesis.timestamp + 1)
    } yield (master, Seq(genesis, issue))

    val invalidReissue = for {
      (genesis, master) <- preconditions
      issue             <- createIssue(master, assetName, assetName, 1, 8, reissuable = false, FeeValidation.FeeUnit * 100000, genesis.timestamp + 1)
      reissue           <- createReissue(master, IssuedAsset(issue.assetId), 1, reissuable = true, (FeeValidation.FeeUnit * 0.9).toLong, genesis.timestamp + 2)
    } yield (master, Seq(genesis, issue, reissue))

    val validReissue = for {
      (genesis, master) <- preconditions
      issue             <- createIssue(master, assetName, assetName, 1, 8, reissuable = false, FeeValidation.FeeUnit * 100000, genesis.timestamp + 1)
      reissue           <- createReissue(master, IssuedAsset(issue.assetId), 1, reissuable = true, FeeValidation.FeeUnit, genesis.timestamp + 2)
    } yield (master, Seq(genesis, issue, reissue))

    val invalidAlias = for {
      (genesis, master) <- preconditions
      alias             <- createAliasGen(master, Alias("123"), (FeeValidation.FeeUnit * 0.9).toLong, genesis.timestamp + 1)
    } yield (master, Seq(genesis, alias))

    val validAlias = for {
      (genesis, master) <- preconditions
      alias             <- createAliasGen(master, Alias("123"), FeeValidation.FeeUnit, genesis.timestamp + 1)
    } yield (master, Seq(genesis, alias))
  }

  val (valid, invalid) = {
    import Setup._
    val valid = Seq(validNftIssue :| "valid nft issue", validIssue :| "valid issue", validReissue :| "valid reissue", validAlias :| "valid alias")
    val invalid = Seq(invalidNftIssue :| "invalid nft issue", invalidIssue :| "invalid issue", invalidReissue :| "invalid reissue", invalidAlias :| "invalid alias")
    (valid, invalid)
  }

  valid.foreach(forAll(_) { case (signer, Seq(genesis, rest @ _*)) =>
    val fs = TestFunctionalitySettings.withFeatures(BlockchainFeatures.IncreaseIssueFee)
    withDomainFS(fs) { d =>
      d.appendBlock(TestBlock.create(Seq(genesis))) shouldBe 'right
      d.appendBlock(TestBlock.create(signer, rest)) shouldBe 'right
    }
  })

  invalid.foreach(forAll(_) { case (signer, Seq(genesis, rest @ _*)) =>
    val fs = TestFunctionalitySettings.withFeatures(BlockchainFeatures.IncreaseIssueFee)
    withDomainFS(fs) { d =>
      d.appendBlock(TestBlock.create(Seq(genesis))) shouldBe 'right
      d.blockchainUpdater.processBlock(TestBlock.create(signer, rest)) should ProduceError.produce("12434")
    }
  })
}
