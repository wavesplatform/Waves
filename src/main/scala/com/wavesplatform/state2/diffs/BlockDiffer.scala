package com.wavesplatform.state2.diffs

import cats._
import cats.Monoid
import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import com.wavesplatform.state2.{BlockDiff, Diff, EffectiveBalanceSnapshot, EqByteArray, Portfolio}
import scorex.account.Account
import scorex.block.Block
import scorex.transaction.{AssetAcc, ValidationError}


object BlockDiffer {

  val emptyDiff: Diff = Monoid[Diff].empty
  val rightEmptyDiff: Either[ValidationError, Diff] = Right(emptyDiff)

  def apply(settings: FunctionalitySettings)(s: StateReader, block: Block): Either[ValidationError, BlockDiff] = {

    val txDiffer = TransactionDiffer(settings, block.timestamp, s.height + 1) _

    val txsDiffEi = block.transactionData.foldLeft(rightEmptyDiff) { case (ei, tx) => ei match {
      case Left(error) => Left(error)
      case Right(diff) =>
        txDiffer(new CompositeStateReader(s, diff.asBlockDiff), tx)
          .map(newDiff => diff.combine(newDiff))
    }
    }

    lazy val accountPortfolioFeesMap: List[(Account, Portfolio)] = block.feesDistribution.toList.map {
      case (AssetAcc(account, maybeAssetId), feeVolume) =>
        account -> (maybeAssetId match {
          case None => Portfolio(feeVolume, feeVolume, Map.empty)
          case Some(assetId) => Portfolio(0L, 0L, Map(EqByteArray(assetId) -> feeVolume))
        })
    }
    lazy val feeDiff = Monoid[Diff].combineAll(accountPortfolioFeesMap.map { case (acc, p) => Diff(Map.empty, Map(acc -> p), Map.empty) })

    txsDiffEi
      .map(_.combine(feeDiff))
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
}
