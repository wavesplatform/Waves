package com.wavesplatform.state2.diffs

import cats._
import cats.Monoid
import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.{BlockDiff, CompositeStateReader, Diff, EqByteArray, Portfolio, StateReader}
import scorex.block.Block
import scorex.transaction.{AssetAcc, ValidationError}


object BlockDiffer {

  val emptyDiff: Diff = implicitly[Monoid[Diff]].empty
  val rightEmptyDiff: Either[ValidationError, Diff] = Right(emptyDiff)

  def apply(s: StateReader, settings: FunctionalitySettings, time: Long)(b: Block): Either[ValidationError, BlockDiff] = {

    //    b.transactionData.foldM(emptyDiff) { case (diff: Diff, tx: Transaction) =>
    //      TransactionDiffer(new CompositeStateReader(s, BlockDiff(diff)), settings, time)(tx)
    //    }

    val txDiffer = TransactionDiffer(settings, time) _

    val txsDiffEi = b.transactionData.foldLeft(rightEmptyDiff) { case (ei, tx) => ei match {
      case Left(error) => Left(error)
      case Right(diff) =>
        txDiffer(new CompositeStateReader(s, BlockDiff(diff)), tx)
          .map(newDiff => diff.combine(newDiff))
    }
    }

    lazy val feeDiff: Diff = b.feesDistribution.map { case (AssetAcc(account, maybeAssetId), feeVolume) =>
      account -> (maybeAssetId match {
        case None => Portfolio(feeVolume, feeVolume, Map.empty)
        case Some(assetId) => Portfolio(0L, 0L, Map(EqByteArray(assetId) -> feeVolume))
      })
    }.foldLeft(emptyDiff) { case (diff, (acc, portfolio)) =>
      diff.combine(Diff(Map.empty, Map(acc -> portfolio), Map.empty))
    }

    txsDiffEi.map(diff => BlockDiff(diff.combine(feeDiff), 1))
  }
}
