package com.wavesplatform.state2.diffs

import cats.Monoid
import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import com.wavesplatform.state2.patch.LeasePatch
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.account.Account
import scorex.block.Block
import scorex.transaction.{AssetAcc, ValidationError}
import scorex.utils.ScorexLogging


object BlockDiffer extends ScorexLogging {

  def right(diff: Diff): Either[ValidationError, Diff] = Right(diff)

  def apply(settings: FunctionalitySettings)(s: StateReader, block: Block): Either[ValidationError, BlockDiff] = {

    val txDiffer = TransactionDiffer(settings, block.timestamp, s.height + 1) _

    val accountPortfolioFeesMap: List[(Account, Portfolio)] = block.feesDistribution.toList.map {
      case (AssetAcc(account, maybeAssetId), feeVolume) =>
        account -> (maybeAssetId match {
          case None => Portfolio(feeVolume, LeaseInfo.empty, Map.empty)
          case Some(assetId) => Portfolio(0L, LeaseInfo.empty, Map(EqByteArray(assetId) -> feeVolume))
        })
    }
    val feeDiff = Monoid[Diff].combineAll(accountPortfolioFeesMap.map { case (acc, p) =>
      Diff(portfolios = Map(acc -> p))
    })

    val txsDiffEi = block.transactionData.foldLeft(right(feeDiff)) { case (ei, tx) => ei.flatMap(diff =>
      txDiffer(new CompositeStateReader(s, diff.asBlockDiff), tx)
        .map(newDiff => diff.combine(newDiff)))
    }

    txsDiffEi
      .map(d => if (s.height + 1 == settings.resetEffectiveBalancesAtHeight)
        Monoid.combine(d, LeasePatch(new CompositeStateReader(s, d.asBlockDiff)))
      else d)
      .map(diff => {
        val effectiveBalanceSnapshots = diff.portfolios
          .filter { case (acc, portfolio) => portfolio.effectiveBalance != 0 }
          .map { case (acc, portfolio) => (acc, s.accountPortfolio(acc).effectiveBalance, portfolio.effectiveBalance) }
          .map { case (acc, oldEffBalance, effBalanceDiff) =>
            EffectiveBalanceSnapshot(acc = acc,
              height = s.height + 1,
              prevEffectiveBalance = if (s.height == 0) (oldEffBalance + effBalanceDiff) else oldEffBalance,
              effectiveBalance = oldEffBalance + effBalanceDiff)
          }
          .toSeq

        BlockDiff(diff, 1, effectiveBalanceSnapshots)
      }
      )
  }


  def unsafeDiffMany(settings: FunctionalitySettings)(s: StateReader, blocks: Seq[Block]): BlockDiff =
    blocks.foldLeft(Monoid[BlockDiff].empty) { case (diff, block) =>
      val blockDiff = apply(settings)(new CompositeStateReader(s, diff), block).explicitGet()
      Monoid[BlockDiff].combine(diff, blockDiff)
  }
}
