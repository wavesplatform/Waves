package com.wavesplatform.state2.diffs

import cats.Monoid
import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.patch.LeasePatch
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.account.Address
import scorex.block.{Block, MicroBlock}
import scorex.transaction.{Signed, Transaction, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.SortedMap

object BlockDiffer extends ScorexLogging {

  def right(diff: Diff): Either[ValidationError, Diff] = Right(diff)

  def fromBlock(settings: FunctionalitySettings, s: StateReader, pervBlockTimestamp: Option[Long], block: Block): Either[ValidationError, BlockDiff] = {
    val feeDistr = if (block.timestamp < settings.enableMicroblocksAfter)
      Some(block.feesDistribution)
    else None
    for {
      _ <- Signed.validateSignatures(block)
      r <- apply(settings, s, pervBlockTimestamp)(block.signerData.generator, feeDistr, block.timestamp, block.transactionData, 1)
    } yield r
  }

  def fromMicroBlock(settings: FunctionalitySettings, s: StateReader, pervBlockTimestamp: Option[Long], micro: MicroBlock, timestamp: Long): Either[ValidationError, BlockDiff] =
    for {
      _ <- Signed.validateSignatures(micro)
      r <- apply(settings, s, pervBlockTimestamp)(micro.generator, None, timestamp, micro.transactionData, 0)
    } yield r


  def unsafeDiffMany(settings: FunctionalitySettings, s: StateReader, prevBlockTimestamp: Option[Long])(blocks: Seq[Block]): BlockDiff =
    blocks.foldLeft((Monoid[BlockDiff].empty, prevBlockTimestamp)) { case ((diff, prev), block) =>
      val blockDiff = fromBlock(settings, new CompositeStateReader(s, diff), prev, block).explicitGet()
      (Monoid[BlockDiff].combine(diff, blockDiff), Some(block.timestamp))
    }._1

  private def apply(settings: FunctionalitySettings, s: StateReader, pervBlockTimestamp: Option[Long])
                   (blockGenerator: Address, maybeFeesDistr: Option[Diff], timestamp: Long, txs: Seq[Transaction], heightDiff: Int): Either[ValidationError, BlockDiff] = {
    val currentBlockHeight = s.height + heightDiff

    val txDiffer = TransactionDiffer(settings, pervBlockTimestamp, timestamp, currentBlockHeight) _

    def txFeeDiffer(tx: Transaction): Map[Address, Portfolio] = Map(blockGenerator ->
      (tx.assetFee match {
        case (Some(asset), fee) =>
          Portfolio(
            balance = 0,
            leaseInfo = LeaseInfo.empty,
            assets = Map(asset -> fee))
        case (None, fee) => Portfolio(
          balance = fee,
          leaseInfo = LeaseInfo.empty,
          assets = Map.empty)
      }))

    val txsDiffEi = maybeFeesDistr match {
      case Some(feedistr) =>
        txs.foldLeft(right(feedistr)) { case (ei, tx) => ei.flatMap(diff =>
          txDiffer(new CompositeStateReader(s, diff.asBlockDiff), tx)
            .map(newDiff => diff.combine(newDiff)))
        }
      case None =>
        txs.foldLeft(right(Diff.empty)) { case (ei, tx) => ei.flatMap(diff =>
          txDiffer(new CompositeStateReader(s, diff.asBlockDiff), tx)
            .map(newDiff => diff.combine(newDiff.copy(portfolios = newDiff.portfolios.combine(txFeeDiffer(tx))))))
        }
    }

    txsDiffEi.map { d =>
      val diff = if (currentBlockHeight == settings.resetEffectiveBalancesAtHeight)
        Monoid.combine(d, LeasePatch(new CompositeStateReader(s, d.asBlockDiff)))
      else d
      val newSnapshots = diff.portfolios
        .collect { case (acc, portfolioDiff) if portfolioDiff.balance != 0 || portfolioDiff.effectiveBalance != 0 =>
          val oldPortfolio = s.accountPortfolio(acc)
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
