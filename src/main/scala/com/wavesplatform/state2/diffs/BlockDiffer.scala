package com.wavesplatform.state2.diffs

import cats.Monoid
import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.patch.LeasePatch
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.account.Account
import scorex.block.Block
import scorex.transaction.{AssetAcc, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.SortedMap


object BlockDiffer extends ScorexLogging {

  def right(diff: Diff): Either[ValidationError, Diff] = Right(diff)

  def apply(settings: FunctionalitySettings)(s: StateReader, block: Block): Either[ValidationError, BlockDiff] = {

    val currentBlockHeight = s.height + 1

    val txDiffer = TransactionDiffer(settings, block.timestamp, currentBlockHeight) _

    val accountPortfolioFeesMap: List[(Account, Portfolio)] = block.feesDistribution.toList.map {
      case (AssetAcc(account, maybeAssetId), feeVolume) =>
        account -> (maybeAssetId match {
          case None => Portfolio(feeVolume, LeaseInfo.empty, Map.empty)
          case Some(assetId) => Portfolio(0L, LeaseInfo.empty, Map(EqByteArray(assetId) -> feeVolume))
        })
    }
    val feeDiff = Monoid[Diff].combineAll(accountPortfolioFeesMap.map { case (acc, p) =>
      Diff.empty.copy(portfolios = Map(acc -> p))
    })

    val txsDiffEi = block.transactionData.foldLeft(right(feeDiff)) { case (ei, tx) => ei.flatMap(diff =>
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
      BlockDiff(diff, 1, newSnapshots)
    }
  }

  def unsafeDiffMany(settings: FunctionalitySettings)(s: StateReader, blocks: Seq[Block]): BlockDiff =
    blocks.foldLeft(Monoid[BlockDiff].empty) { case (diff, block) =>
      val blockDiff = apply(settings)(new CompositeStateReader(s, diff), block).explicitGet()
      Monoid[BlockDiff].combine(diff, blockDiff)
    }
}
