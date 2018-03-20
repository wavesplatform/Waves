package com.wavesplatform.state2.diffs

import cats.Monoid
import cats.implicits._
import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.patch.{CancelAllLeases, CancelLeaseOverflow}
import com.wavesplatform.state2.reader.CompositeStateReader.composite
import com.wavesplatform.state2.reader.SnapshotStateReader
import scorex.account.Address
import scorex.block.{Block, MicroBlock}
import scorex.transaction.ValidationError.ActivationError
import scorex.transaction.{Transaction, ValidationError}
import scorex.utils.ScorexLogging

object BlockDiffer extends ScorexLogging with Instrumented {

  def right(diff: Diff): Either[ValidationError, Diff] = Right(diff)

  def fromBlock(settings: FunctionalitySettings, fp: FeatureProvider, s: SnapshotStateReader, maybePrevBlock: Option[Block], block: Block): Either[ValidationError, Diff] = {
    val blockSigner = block.signerData.generator.toAddress
    val stateHeight = s.height

    // height switch is next after activation
    val ng4060switchHeight = fp.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)

    lazy val prevBlockFeeDistr: Option[Diff] =
      if (stateHeight > ng4060switchHeight)
        maybePrevBlock
          .map(prevBlock => Diff.empty.copy(
            portfolios = Map(blockSigner -> prevBlock.prevBlockFeePart())))
      else None

    lazy val currentBlockFeeDistr =
      if (stateHeight < ng4060switchHeight)
        Some(Diff.empty.copy(portfolios = Map(blockSigner -> block.feesPortfolio())))
      else
        None

    val prevBlockTimestamp = maybePrevBlock.map(_.timestamp)
    for {
      _ <- block.signaturesValid()
      r <- apply(settings, s, fp, prevBlockTimestamp)(block.signerData.generator, prevBlockFeeDistr, currentBlockFeeDistr, block.timestamp, block.transactionData, 1)
    } yield r
  }

  def fromMicroBlock(settings: FunctionalitySettings, fp: FeatureProvider, s: SnapshotStateReader, prevBlockTimestamp: Option[Long], micro: MicroBlock, timestamp: Long): Either[ValidationError, Diff] = {
    for {
      // microblocks are processed within block which is next after 40-only-block which goes on top of activated height
      _ <- Either.cond(fp.activatedFeatures().contains(BlockchainFeatures.NG.id), (), ActivationError(s"MicroBlocks are not yet activated"))
      _ <- micro.signaturesValid()
      r <- apply(settings, s, fp, prevBlockTimestamp)(micro.sender, None, None, timestamp, micro.transactionData, 0)
    } yield r
  }

  private def apply(settings: FunctionalitySettings, s: SnapshotStateReader, fp: FeatureProvider, prevBlockTimestamp: Option[Long])
                   (blockGenerator: Address, prevBlockFeeDistr: Option[Diff], currentBlockFeeDistr: Option[Diff],
                    timestamp: Long, txs: Seq[Transaction], heightDiff: Int): Either[ValidationError, Diff] = {
    val currentBlockHeight = s.height + heightDiff
    val txDiffer = TransactionDiffer(settings, prevBlockTimestamp, timestamp, currentBlockHeight) _

    val txsDiffEi = currentBlockFeeDistr match {
      case Some(feedistr) =>
        txs.foldLeft(right(Monoid.combine(prevBlockFeeDistr.orEmpty, feedistr))) { case (ei, tx) => ei.flatMap(diff =>
          txDiffer(composite(s, diff), fp, tx)
            .map(newDiff => diff.combine(newDiff)))
        }
      case None =>
        txs.foldLeft(right(prevBlockFeeDistr.orEmpty)) { case (ei, tx) => ei.flatMap(diff =>
          txDiffer(composite(s, diff), fp, tx)
            .map(newDiff => diff.combine(newDiff.copy(portfolios = newDiff.portfolios.combine(Map(blockGenerator -> tx.feeDiff()).mapValues(_.multiply(Block.CurrentBlockFeePart)))))))
        }
    }

    txsDiffEi.map { d =>
      val diffWithCancelledLeases = if (currentBlockHeight == settings.resetEffectiveBalancesAtHeight)
        Monoid.combine(d, CancelAllLeases(composite(s, d)))
      else d

      val diffWithLeasePatches = if (currentBlockHeight == settings.blockVersion3AfterHeight)
        Monoid.combine(diffWithCancelledLeases, CancelLeaseOverflow(composite(s, diffWithCancelledLeases)))
      else diffWithCancelledLeases

      diffWithLeasePatches
    }
  }
}
