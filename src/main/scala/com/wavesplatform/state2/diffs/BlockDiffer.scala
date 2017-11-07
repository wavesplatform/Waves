package com.wavesplatform.state2.diffs

import cats.Monoid
import cats.data.{NonEmptyList => NEL}
import cats.implicits._
import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.patch.LeasePatch
import com.wavesplatform.state2.reader.CompositeStateReader.composite
import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import scorex.account.Address
import scorex.block.{Block, MicroBlock}
import scorex.transaction.ValidationError.ActivationError
import scorex.transaction.{History, Transaction, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.SortedMap
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object BlockDiffer extends ScorexLogging with Instrumented {

  private implicit val scheduler: SchedulerService = monix.execution.Scheduler.computation(name = "block-deser",
    reporter = com.wavesplatform.utils.UncaughtExceptionsToLogReporter)

  def right(diff: Diff): Either[ValidationError, Diff] = Right(diff)

  def fromBlock(settings: FunctionalitySettings, fp: FeatureProvider, s: SnapshotStateReader, maybePrevBlock: Option[Block], block: Block): Either[ValidationError, BlockDiff] = {
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
      r <- apply(settings, s, prevBlockTimestamp)(block.signerData.generator, prevBlockFeeDistr, currentBlockFeeDistr, block.timestamp, block.transactionData, 1)
    } yield r
  }

  def fromMicroBlock(settings: FunctionalitySettings, fp: FeatureProvider, s: SnapshotStateReader, pervBlockTimestamp: Option[Long], micro: MicroBlock, timestamp: Long): Either[ValidationError, BlockDiff] = {
    for {
      // microblocks are processed within block which is next after 40-only-block which goes on top of activated height
      _ <- Either.cond(fp.featureActivationHeight(BlockchainFeatures.NG.id).exists(s.height > _), (), ActivationError(s"MicroBlocks are not yet activated, current height=${s.height}"))
      _ <- micro.signaturesValid()
      r <- apply(settings, s, pervBlockTimestamp)(micro.generator, None, None, timestamp, micro.transactionData, 0)
    } yield r
  }

  def unsafeDiffMany(settings: FunctionalitySettings, fp: FeatureProvider, s: SnapshotStateReader, prevBlock: Option[Block], maxTxsInChunk: Int)
                    (blocks: Seq[Block]): NEL[BlockDiff] =
    blocks.foldLeft((NEL.one(BlockDiff.empty), prevBlock)) { case ((diffs, prev), block) =>
      val blockDiff = fromBlock(settings, fp, composite(diffs, s), prev, block).explicitGet()
      (prependCompactBlockDiff(blockDiff, diffs, maxTxsInChunk), Some(block))
    }._1

  def unsafeDiffByRange(fs: FunctionalitySettings, fp: FeatureProvider, h: History, maxTransactionsPerChunk: Int)(state: SnapshotStateReader, upto: Int): NEL[BlockDiff] = {
    val from = state.height + 1
    val blocks = measureLog(s"Reading blocks from $from up upto $upto") {
      Await.result(Task.wander(Range(from, upto).map(h.blockBytes))(b => Task(Block.parseBytes(b.get).get)).runAsync, Duration.Inf)
    }
    measureLog(s"Building diff from $from up to $upto") {
      BlockDiffer.unsafeDiffMany(fs, fp, state, h.blockAt(from - 1), maxTransactionsPerChunk)(blocks)
    }
  }

  private def apply(settings: FunctionalitySettings, s: SnapshotStateReader, pervBlockTimestamp: Option[Long])
                   (blockGenerator: Address, prevBlockFeeDistr: Option[Diff], currentBlockFeeDistr: Option[Diff],
                    timestamp: Long, txs: Seq[Transaction], heightDiff: Int): Either[ValidationError, BlockDiff] = {
    val currentBlockHeight = s.height + heightDiff
    val txDiffer = TransactionDiffer(settings, pervBlockTimestamp, timestamp, currentBlockHeight) _

    val txsDiffEi = currentBlockFeeDistr match {
      case Some(feedistr) =>
        txs.foldLeft(right(Monoid.combine(prevBlockFeeDistr.orEmpty, feedistr))) { case (ei, tx) => ei.flatMap(diff =>
          txDiffer(composite(diff.asBlockDiff, s), tx)
            .map(newDiff => diff.combine(newDiff)))
        }
      case None =>
        txs.foldLeft(right(prevBlockFeeDistr.orEmpty)) { case (ei, tx) => ei.flatMap(diff =>
          txDiffer(composite(diff.asBlockDiff, s), tx)
            .map(newDiff => diff.combine(newDiff.copy(portfolios = newDiff.portfolios.combine(Map(blockGenerator -> tx.feeDiff()).mapValues(_.multiply(Block.CurrentBlockFeePart)))))))
        }
    }

    txsDiffEi.map { d =>
      val diff = if (currentBlockHeight == settings.resetEffectiveBalancesAtHeight)
        Monoid.combine(d, LeasePatch(composite(d.asBlockDiff, s)))
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
