package com.wavesplatform.state2.diffs

import cats.Monoid
import cats.implicits._
import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.patch.LeasePatch
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.account.Address
import scorex.block.{Block, MicroBlock}
import scorex.transaction.{Signed, Transaction, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.SortedMap

object BlockDiffer extends ScorexLogging with Instrumented {

  def right(diff: Diff): Either[ValidationError, Diff] = Right(diff)

  def fromBlock(settings: FunctionalitySettings, featureProvider: FeatureProvider, s: StateReader, maybePrevBlock: Option[Block], block: Block): Either[ValidationError, BlockDiff] = {
    val blockSigner = block.signerData.generator.toAddress
    val stateHeight = s.height

    // height switch is next after activation
    val ng4060switchHeight = featureProvider.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)

    lazy val prevBlockFeeDistr: Option[Diff] =
      if (stateHeight > ng4060switchHeight)
        maybePrevBlock
          .map(prevBlock => Diff.empty.copy(
            portfolios = Map(blockSigner -> prevBlock.prevBlockFeePart)))
      else None

    lazy val currentBlockFeeDistr =
      if (stateHeight < ng4060switchHeight)
        Some(Diff.empty.copy(portfolios = Map(blockSigner -> block.feesPortfolio)))
      else
        None

    val prevBlockTimestamp = maybePrevBlock.map(_.timestamp)
    for {
      _ <- Signed.validateSignatures(block)
      r <- apply(settings, s, prevBlockTimestamp)(block.signerData.generator, prevBlockFeeDistr, currentBlockFeeDistr, block.timestamp, block.transactionData, 1)
    } yield r
  }

  def fromMicroBlock(settings: FunctionalitySettings, s: StateReader, pervBlockTimestamp: Option[Long], micro: MicroBlock, timestamp: Long): Either[ValidationError, BlockDiff] = {
    for {
      _ <- Signed.validateSignatures(micro)
      r <- apply(settings, s, pervBlockTimestamp)(micro.generator, None, None, timestamp, micro.transactionData, 0)
    } yield r
  }

  def unsafeDiffMany(settings: FunctionalitySettings, fp: FeatureProvider, s: StateReader, prevBlock: Option[Block])(blocks: Seq[Block]): BlockDiff =
    blocks.foldLeft((Monoid[BlockDiff].empty, prevBlock)) { case ((diff, prev), block) =>
      val blockDiff = fromBlock(settings, fp, new CompositeStateReader(s, diff), prev, block).explicitGet()
      (Monoid[BlockDiff].combine(diff, blockDiff), Some(block))
    }._1

  private def apply(settings: FunctionalitySettings, s: StateReader, pervBlockTimestamp: Option[Long])
                   (blockGenerator: Address, prevBlockFeeDistr: Option[Diff], currentBlockFeeDistr: Option[Diff],
                    timestamp: Long, txs: Seq[Transaction], heightDiff: Int): Either[ValidationError, BlockDiff] = {
    val currentBlockHeight = s.height + heightDiff
    val txDiffer = TransactionDiffer(settings, pervBlockTimestamp, timestamp, currentBlockHeight) _

    val txsDiffEi = currentBlockFeeDistr match {
      case Some(feedistr) =>
        txs.foldLeft(right(Monoid.combine(prevBlockFeeDistr.orEmpty, feedistr))) { case (ei, tx) => ei.flatMap(diff =>
          txDiffer(new CompositeStateReader(s, diff.asBlockDiff), tx)
            .map(newDiff => diff.combine(newDiff)))
        }
      case None =>
        txs.foldLeft(right(prevBlockFeeDistr.orEmpty)) { case (ei, tx) => ei.flatMap(diff =>
          txDiffer(new CompositeStateReader(s, diff.asBlockDiff), tx)
            .map(newDiff => diff.combine(newDiff.copy(portfolios = newDiff.portfolios.combine(Map(blockGenerator -> tx.feeDiff()).mapValues(_.multiply(Block.CurrentBlockFeePart)))))))
        }
    }

    txsDiffEi.map { d =>
      val diff = if (currentBlockHeight == settings.resetEffectiveBalancesAtHeight)
        Monoid.combine(d, LeasePatch(new CompositeStateReader(s, d.asBlockDiff)))
      else d
      val newSnapshots = diff.portfolios
        .collect { case (acc, portfolioDiff) if portfolioDiff.balance != 0 || portfolioDiff.effectiveBalance != 0 =>
          val oldPortfolio = s.partialPortfolio(acc, Set.empty[ByteStr])
          if (s.lastUpdateHeight(acc).contains(currentBlockHeight)) {
            throw new Exception(s"CRITICAL: attempting to build a circular reference in snapshot list. " +
              s"acc=$acc, currentBlockHeight=$currentBlockHeight")
          }
          acc -> SortedMap(currentBlockHeight -> Snapshot(
            prevHeight = s.lastUpdateHeight(acc).getOrElse(0),
            balance = oldPortfolio.balance + portfolioDiff.balance,
            effectiveBalance = oldPortfolio.effectiveBalance + portfolioDiff.effectiveBalance))
        }
      BlockDiff(diff, heightDiff, newSnapshots)
    }
  }
}
