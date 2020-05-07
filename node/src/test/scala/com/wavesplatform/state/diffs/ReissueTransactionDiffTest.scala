package com.wavesplatform.state.diffs

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.assets.{IssueTransaction, ReissueTransaction}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Assertion, EitherValues, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ReissueTransactionDiffTest
    extends PropSpec
    with PropertyChecks
    with Matchers
    with TransactionGen
    with WithState
    with EitherValues
    with NoShrink {
  import ReissueTransactionDiffTest._

  private val beforeActivationScenario =
    for {
      (issuer, b1) <- genesis
      issue        <- issueTx(issuer)
      reissue      <- reissueTx(issuer, IssuedAsset(issue.assetId), BeforeActivationFee)
      b2 = TestBlock.create(
        ntpNow,
        b1.id(),
        Seq(issue),
        issuer
      )
    } yield (Seq(b1, b2), reissue)

  property("Reissue transaction's fee before feature activation is 1 WAVES") {
    forAll(beforeActivationScenario) {
      case (bs, txs) =>
        checkFee(bs, txs) {
          case (result, lessResult, moreResult) =>
            result shouldBe 'right
            lessResult shouldBe 'left
            moreResult shouldBe 'right
        }
    }
  }

  private val afterActivationScenario =
    for {
      (issuer, b1) <- genesis
      issue1       <- issueTx(issuer)
      issue2       <- issueTx(issuer)
      reissue      <- reissueTx(issuer, IssuedAsset(issue1.assetId), AfterActivationFee)
      b2 = TestBlock.create(
        ntpNow,
        b1.id(),
        Seq(issue1),
        issuer
      )
      b3 = TestBlock.create(
        ntpNow,
        b2.id(),
        Seq(issue2),
        issuer
      )
    } yield (Seq(b1, b2, b3), reissue)

  property("Reissue transaction's fee after feature activation is 0.001 WAVES") {
    forAll(afterActivationScenario) {
      case (bs, txs) =>
        checkFee(bs, txs) {
          case (result, lessResult, moreResult) =>
            result shouldBe 'right
            lessResult shouldBe 'left
            moreResult shouldBe 'right
        }
    }
  }

  private def checkFee(preconditions: Seq[Block], txs: TransactionsForCheck)(f: ValidationResults => Assertion): Unit =
    withLevelDBWriter(fs) { blockchain =>
      preconditions.foreach { block =>
        val BlockDiffer.Result(preconditionDiff, preconditionFees, totalFee, _, _) =
          BlockDiffer.fromBlock(blockchain, blockchain.lastBlock, block, MiningConstraint.Unlimited).explicitGet()
        blockchain.append(preconditionDiff, preconditionFees, totalFee, None, block.header.generationSignature, block)
      }
      f((FeeValidation(blockchain, txs._1), FeeValidation(blockchain, txs._2), FeeValidation(blockchain, txs._3)))
    }

  private def genesis: Gen[(KeyPair, Block)] =
    for {
      issuer <- accountGen
      block = TestBlock.create(
        ntpNow,
        Seq(GenesisTransaction.create(issuer.toAddress, Constants.TotalWaves * Constants.UnitsInWave, ntpNow).explicitGet())
      )
    } yield (issuer, block)

  private def issueTx(issuer: KeyPair): Gen[IssueTransaction] =
    for {
      name        <- genBoundedString(IssueTransaction.MinAssetNameLength, IssueTransaction.MaxAssetNameLength)
      description <- genBoundedString(0, IssueTransaction.MaxAssetNameLength)
      quantity    <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
      decimals    <- Gen.choose(0: Byte, 8: Byte)
      fee         <- Gen.choose(MinIssueFee, 2 * MinIssueFee)
      tx          <- createLegacyIssue(issuer, name.getBytes, description.getBytes, quantity, decimals, true, fee, ntpNow)
    } yield tx

  private def reissueTx(reissuer: KeyPair, assetId: IssuedAsset, fee: Long): Gen[(ReissueTransaction, ReissueTransaction, ReissueTransaction)] =
    for {
      quantity   <- Gen.choose(Long.MaxValue / 200, Long.MaxValue / 100)
      reissuable <- Arbitrary.arbitrary[Boolean]
      lessFee    <- Gen.choose(1, fee - 1)
      moreFee    <- Gen.choose(fee + 1, fee * 2)
      tx         <- createReissue(reissuer, assetId, quantity, reissuable, fee, ntpNow)
      lessTx     <- createReissue(reissuer, assetId, quantity, reissuable, lessFee, ntpNow)
      moreTx     <- createReissue(reissuer, assetId, quantity, reissuable, moreFee, ntpNow)
    } yield (tx, lessTx, moreTx)
}

object ReissueTransactionDiffTest {
  type TransactionsForCheck = (ReissueTransaction, ReissueTransaction, ReissueTransaction)
  type ValidationResults    = (Either[ValidationError, Unit], Either[ValidationError, Unit], Either[ValidationError, Unit])

  val fs: FunctionalitySettings =
    TestFunctionalitySettings.Enabled.copy(
      featureCheckBlocksPeriod = 1,
      blocksForFeatureActivation = 1,
      preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures ++ Seq(
        BlockchainFeatures.FeeSponsorship.id           -> 0,
        BlockchainFeatures.BlockV5.id -> 3
      )
    )

  val BeforeActivationFee: Long = 1 * Constants.UnitsInWave
  val AfterActivationFee: Long  = 100000
}
