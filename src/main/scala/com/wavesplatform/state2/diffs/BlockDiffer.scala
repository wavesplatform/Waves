package com.wavesplatform.state2.diffs

import cats.Monoid
import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.patch.LeasePatch
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.block.Block
import scorex.transaction.{History, Signed, Transaction, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.SortedMap

object BlockDiffer extends ScorexLogging {

  def right(diff: Diff): Either[ValidationError, Diff] = Right(diff)

  def fromBlock(settings: FunctionalitySettings, s: StateReader, h: History)(block: Block): Either[ValidationError, BlockDiff] =
    Signed.validateSignatures(block).flatMap { _ => apply(settings, s, h)(block.feesDistribution, block.timestamp, block.transactionData, 1) }

  def unsafeDiffMany(settings: FunctionalitySettings, s: StateReader, h: History)(blocks: Seq[Block]): BlockDiff =
    blocks.foldLeft(Monoid[BlockDiff].empty) { case (diff, block) =>
      val blockDiff = fromBlock(settings, new CompositeStateReader(s, diff), h)(block).explicitGet()
      Monoid[BlockDiff].combine(diff, blockDiff)
    }

  private def apply(settings: FunctionalitySettings, s: StateReader, h: History)(feesDistribution: Diff, timestamp: Long, txs: Seq[Transaction], heightDiff: Int) = {
    val currentBlockHeight = s.height + 1

    val txDiffer = TransactionDiffer(settings, h.lastBlock.map(_.timestamp), timestamp, currentBlockHeight) _

    val txsDiffEi = txs.foldLeft(right(feesDistribution)) { case (ei, tx) => ei.flatMap(diff =>
      txDiffer(new CompositeStateReader(s, diff.asBlockDiff), tx)
        .map(newDiff => diff.combine(newDiff)))
    }

    txsDiffEi.map { d =>
      val diff = if (currentBlockHeight == settings.resetEffectiveBalancesAtHeight)
        Monoid.combine(d, LeasePatch(new CompositeStateReader(s, d.asBlockDiff)))
      else d
      val newSnapshots = diff.portfolios
        .collect { case (acc, portfolioDiff) if (portfolioDiff.balance != 0 || portfolioDiff.effectiveBalance != 0) =>
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
