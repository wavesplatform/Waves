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

  val emptyDiff: Diff = Monoid[Diff].empty
  val rightEmptyDiff: Either[ValidationError, Diff] = Right(emptyDiff)

  def apply(settings: FunctionalitySettings)(s: StateReader, block: Block): Either[ValidationError, BlockDiff] = {

    val currentBlockHeight = s.height + 1

    val txDiffer = TransactionDiffer(settings, block.timestamp, currentBlockHeight) _

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
          case None => Portfolio(feeVolume, LeaseInfo.empty, Map.empty)
          case Some(assetId) => Portfolio(0L, LeaseInfo.empty, Map(EqByteArray(assetId) -> feeVolume))
        })
    }
    lazy val feeDiff = Monoid[Diff].combineAll(accountPortfolioFeesMap.map { case (acc, p) =>
      new Diff(Map.empty, Map(acc -> p), Map.empty, Map.empty, Map.empty, Map.empty, Seq.empty)
    })

    txsDiffEi
      .map(_.combine(feeDiff))
      .map(d => if (currentBlockHeight == settings.resetEffectiveBalancesAtHeight)
        Monoid.combine(d, LeasePatch(new CompositeStateReader(s, d.asBlockDiff)))
      else d)
      .map(diff => {
        val newSnapshots = diff.portfolios
          .filter { case (acc, portfolioDiff) => portfolioDiff.balance != 0 || portfolioDiff.effectiveBalance != 0 }
          .map { case (acc, portfolioDiff) =>
            val oldPortfolio = s.accountPortfolio(acc)
            acc -> Map(currentBlockHeight -> Snapshot(
              prevHeight = s.lastUpdateHeight(acc).getOrElse(0),
              balance = oldPortfolio.balance + portfolioDiff.balance,
              effectiveBalance = oldPortfolio.effectiveBalance + portfolioDiff.effectiveBalance))
          }
        BlockDiff(diff, 1, newSnapshots)
      }
      )
  }


  def unsafeDiffMany(settings: FunctionalitySettings, log: (String) => Unit = _ => ())(s: StateReader, blocks: Seq[Block]): BlockDiff = {
    val r = blocks.foldLeft(Monoid[BlockDiff].empty) { case (diff, block) =>
      val blockDiff = apply(settings)(new CompositeStateReader(s, diff), block).explicitGet()
      if (diff.heightDiff % 1000 == 0) {
        log(s"Rebuilt ${diff.heightDiff} blocks out of ${blocks.size}")
      }
      Monoid[BlockDiff].combine(diff, blockDiff)
    }
    log(s"Rebuild of ${blocks.size} completed")
    r
  }
}
