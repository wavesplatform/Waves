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
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers, TxVersion}
import com.wavesplatform.transaction.assets.ReissueTransaction
import org.scalatest.EitherValues

class ReissueTransactionDiffTest extends PropSpec with WithState with EitherValues {
  import ReissueTransactionDiffTest.*

  private val beforeActivationScenario = {
    val (issuer, b1) = genesis
    val issue        = TxHelpers.issue(issuer, version = TxVersion.V1)
    reissueTx(issuer, issue.asset, BeforeActivationFee).map { reissue =>
      val b2 = TestBlock
        .create(
          ntpNow,
          b1.id(),
          Seq(issue),
          issuer
        )
        .block

      (Seq(b1, b2), reissue)
    }
  }

  property("Reissue transaction's fee before feature activation is 1 WAVES") {
    beforeActivationScenario.foreach { case (bs, txs) =>
      checkFee(bs, txs) { case (result, lessResult, moreResult) =>
        result.explicitGet()
        lessResult.left.value
        moreResult.explicitGet()
      }
    }
  }

  private val afterActivationScenario = {
    val (issuer, b1) = genesis
    val issue1       = TxHelpers.issue(issuer, name = "asset1", version = TxVersion.V1)
    val issue2       = TxHelpers.issue(issuer, name = "asset2", version = TxVersion.V1)

    reissueTx(issuer, issue1.asset, AfterActivationFee).map { reissue =>
      val b2 = TestBlock
        .create(
          ntpNow,
          b1.id(),
          Seq(issue1),
          issuer
        )
        .block
      val b3 = TestBlock
        .create(
          ntpNow,
          b2.id(),
          Seq(issue2),
          issuer
        )
        .block

      (Seq(b1, b2, b3), reissue)
    }
  }

  property("Reissue transaction's fee after feature activation is 0.001 WAVES") {
    afterActivationScenario.foreach { case (bs, txs) =>
      checkFee(bs, txs) { case (result, lessResult, moreResult) =>
        result.explicitGet()
        lessResult.left.value
        moreResult.explicitGet()
      }
    }
  }

  private def checkFee(preconditions: Seq[Block], txs: TransactionsForCheck)(f: ValidationResults => Any): Unit =
    withRocksDBWriter(fs) { blockchain =>
      preconditions.foreach { block =>
        val BlockDiffer.Result(preconditionDiff, preconditionFees, totalFee, _, _, computedStateHash) =
          BlockDiffer
            .fromBlock(blockchain, blockchain.lastBlock, block, None, MiningConstraint.Unlimited, block.header.generationSignature)
            .explicitGet()
        blockchain.append(preconditionDiff, preconditionFees, totalFee, None, block.header.generationSignature, computedStateHash, block)
      }
      f((FeeValidation(blockchain, txs._1), FeeValidation(blockchain, txs._2), FeeValidation(blockchain, txs._3)))
    }

  private def genesis: (KeyPair, Block) = {
    val issuer = TxHelpers.signer(1)
    val block = TestBlock
      .create(
        ntpNow,
        Seq(GenesisTransaction.create(issuer.toAddress, Constants.TotalWaves * Constants.UnitsInWave, ntpNow).explicitGet())
      )
      .block

    (issuer, block)
  }

  private def reissueTx(reissuer: KeyPair, assetId: IssuedAsset, fee: Long): Seq[(ReissueTransaction, ReissueTransaction, ReissueTransaction)] = {
    for {
      tx <- Seq(
        TxHelpers.reissue(assetId, reissuer, Long.MaxValue / 100, fee = fee),
        TxHelpers.reissue(assetId, reissuer, Long.MaxValue / 100, fee = fee, version = TxVersion.V1)
      )
      lessTx <- Seq(
        TxHelpers.reissue(assetId, reissuer, Long.MaxValue / 100, fee = fee - 1),
        TxHelpers.reissue(assetId, reissuer, Long.MaxValue / 100, fee = fee - 1, version = TxVersion.V1)
      )
      moreTx <- Seq(
        TxHelpers.reissue(assetId, reissuer, Long.MaxValue / 100, fee = fee + 1),
        TxHelpers.reissue(assetId, reissuer, Long.MaxValue / 100, fee = fee + 1, version = TxVersion.V1)
      )
    } yield (tx, lessTx, moreTx)
  }
}

object ReissueTransactionDiffTest {
  type TransactionsForCheck = (ReissueTransaction, ReissueTransaction, ReissueTransaction)
  type ValidationResults    = (Either[ValidationError, Unit], Either[ValidationError, Unit], Either[ValidationError, Unit])

  val fs: FunctionalitySettings =
    TestFunctionalitySettings.Enabled.copy(
      featureCheckBlocksPeriod = 1,
      blocksForFeatureActivation = 1,
      preActivatedFeatures = TestFunctionalitySettings.Enabled.preActivatedFeatures ++ Seq(
        BlockchainFeatures.FeeSponsorship.id -> 0,
        BlockchainFeatures.BlockV5.id        -> 3
      )
    )

  val BeforeActivationFee: Long = 1 * Constants.UnitsInWave
  val AfterActivationFee: Long  = 100000
}
