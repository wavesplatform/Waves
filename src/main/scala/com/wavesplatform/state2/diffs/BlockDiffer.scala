package com.wavesplatform.state2.diffs

import cats.Monoid
import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.patch.LeasePatch
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.account.Address
import scorex.block.Block
import scorex.transaction.{Signed, Transaction, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.SortedMap

object BlockDiffer extends ScorexLogging {

  def right(diff: Diff): Either[ValidationError, Diff] = Right(diff)

  def fromBlock(settings: FunctionalitySettings, s: StateReader)(block: Block): Either[ValidationError, BlockDiff] =
    Signed.validateSignatures(block).flatMap { _ => apply(settings, s)(block.signerData.generator, block.feesDistribution, block.timestamp, block.transactionData, 1) }

  def fromLiquidBlock(settings: FunctionalitySettings, s: StateReader)(block: Block): Either[ValidationError, BlockDiff] =
    Signed.validateSignatures(block).flatMap { _ => apply(settings, s)(block.signerData.generator, block.feesDistribution, block.timestamp, block.transactionData, 0) }

  def unsafeDiffMany(settings: FunctionalitySettings, s: StateReader)(blocks: Seq[Block]): BlockDiff =
    blocks.foldLeft(Monoid[BlockDiff].empty) { case (diff, block) =>
      val blockDiff = fromBlock(settings, new CompositeStateReader(s, diff))(block).explicitGet()
      Monoid[BlockDiff].combine(diff, blockDiff)
    }

  private def apply(settings: FunctionalitySettings, s: StateReader)(blockGenerator: Address, feesDistribution: Diff, timestamp: Long, txs: Seq[Transaction], heightDiff: Int) = {
    val currentBlockHeight = s.height + 1

    val txDiffer = TransactionDiffer(settings, timestamp, currentBlockHeight) _
    def txFeeDiffer(tx: Transaction): Diff = tx.assetFee match {
      case (Some(asset), fee) =>
        Diff(currentBlockHeight, tx, portfolios = Map(blockGenerator -> Portfolio(
          balance = 0,
          leaseInfo = LeaseInfo.empty,
          assets = Map(asset -> fee))))
      case (None, fee) => Diff(currentBlockHeight, tx, portfolios = Map(blockGenerator -> Portfolio(
        balance = fee,
        leaseInfo = LeaseInfo.empty,
        assets = Map.empty)))
    }

    val txsDiffEi = if (timestamp < settings.giveBlockFeeToGeneratorBeforeApplyUntil) {
      txs.foldLeft(right(feesDistribution)) { case (ei, tx) => ei.flatMap(diff =>
        txDiffer(new CompositeStateReader(s, diff.asBlockDiff), tx)
          .map(newDiff => diff.combine(newDiff)))
      }
    } else {
      txs.foldLeft(right(Diff.empty)) { case (ei, tx) => ei.flatMap(diff =>
        txDiffer(new CompositeStateReader(s, diff.asBlockDiff), tx)
          .map(newDiff =>
            diff.combine(newDiff)
                .combine(txFeeDiffer(tx))
          ))
      }
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
