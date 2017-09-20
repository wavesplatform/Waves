package com.wavesplatform.state2.diffs

import cats.Monoid
import cats.implicits._
import com.wavesplatform.features.Functionalities
import com.wavesplatform.state2._
import com.wavesplatform.state2.patch.LeasePatch
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.block.Block
import scorex.transaction.{Signed, Transaction, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.SortedMap

object BlockDiffer extends ScorexLogging {

  def right(diff: Diff): Either[ValidationError, Diff] = Right(diff)

  def fromBlock(fn: Functionalities, s: StateReader, pervBlockTimestamp: Option[Long])(block: Block): Either[ValidationError, BlockDiff] =
    Signed.validateSignatures(block).flatMap { _ => apply(fn, s, pervBlockTimestamp)(block.feesDistribution, block.timestamp, block.transactionData, 1, block.supportedFeaturesIds) }

  def unsafeDiffMany(fn: Functionalities, s: StateReader, prevBlockTimestamp: Option[Long])(blocks: Seq[Block]): BlockDiff =
    blocks.foldLeft((Monoid[BlockDiff].empty, prevBlockTimestamp)) { case ((diff, prev), block) =>
      val blockDiff = fromBlock(fn, new CompositeStateReader(s, diff), prev)(block).explicitGet()
      (Monoid[BlockDiff].combine(diff, blockDiff), Some(block.timestamp))
    }._1

  private def apply(fn: Functionalities, s: StateReader, pervBlockTimestamp: Option[Long])(feesDistribution: Diff, timestamp: Long, txs: Seq[Transaction], heightDiff: Int, features: Set[Short]) = {
    val currentBlockHeight = s.height + 1

    val txDiffer = TransactionDiffer(fn, pervBlockTimestamp, timestamp, currentBlockHeight) _

    val txsDiffEi = txs.foldLeft(right(feesDistribution)) { case (ei, tx) => ei.flatMap(diff =>
      txDiffer(new CompositeStateReader(s, diff.asBlockDiff), tx)
        .map(newDiff => diff.combine(newDiff)))
    }

    txsDiffEi.map { d =>
      val diff = if (fn.resetEffectiveBalancesAt.check(currentBlockHeight).isRight)
        Monoid.combine(d, LeasePatch(new CompositeStateReader(s, d.asBlockDiff)))
      else d
      val newSnapshots = diff.portfolios
        .collect { case (acc, portfolioDiff) if portfolioDiff.balance != 0 || portfolioDiff.effectiveBalance != 0 =>
          val oldPortfolio = s.accountPortfolio(acc)
          acc -> SortedMap(currentBlockHeight -> Snapshot(
            prevHeight = s.lastUpdateHeight(acc).getOrElse(0),
            balance = oldPortfolio.balance + portfolioDiff.balance,
            effectiveBalance = oldPortfolio.effectiveBalance + portfolioDiff.effectiveBalance))
        }
      BlockDiff(diff, heightDiff, newSnapshots)
    }
  }
}
