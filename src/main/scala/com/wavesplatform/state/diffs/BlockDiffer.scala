package com.wavesplatform.state.diffs

import cats.Monoid
import cats.implicits._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.state.patch.{CancelAllLeases, CancelInvalidLeaseIn, CancelLeaseOverflow}
import com.wavesplatform.state.reader.CompositeBlockchain.composite
import scorex.account.Address
import scorex.block.{Block, MicroBlock}
import scorex.transaction.ValidationError.ActivationError
import scorex.transaction.{Transaction, ValidationError}
import scorex.utils.ScorexLogging

object BlockDiffer extends ScorexLogging with Instrumented {

  def right(diff: Diff): Either[ValidationError, Diff] = Right(diff)

  def fromBlock(settings: FunctionalitySettings,
                blockchain: Blockchain,
                maybePrevBlock: Option[Block],
                block: Block): Either[ValidationError, Diff] = {
    val blockSigner = block.signerData.generator.toAddress
    val stateHeight = blockchain.height

    // height switch is next after activation
    val ng4060switchHeight = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)

    lazy val prevBlockFeeDistr: Option[Diff] =
      if (stateHeight > ng4060switchHeight)
        maybePrevBlock
          .map(prevBlock => Diff.empty.copy(portfolios = Map(blockSigner -> prevBlock.prevBlockFeePart())))
      else None

    lazy val currentBlockFeeDistr =
      if (stateHeight < ng4060switchHeight)
        Some(Diff.empty.copy(portfolios = Map(blockSigner -> block.feesPortfolio())))
      else
        None

    val prevBlockTimestamp = maybePrevBlock.map(_.timestamp)
    for {
      _ <- block.signaturesValid()
      r <- apply(settings, blockchain, prevBlockTimestamp)(block.signerData.generator,
                                                           prevBlockFeeDistr,
                                                           currentBlockFeeDistr,
                                                           block.timestamp,
                                                           block.transactionData,
                                                           1)
    } yield r
  }

  def fromMicroBlock(settings: FunctionalitySettings,
                     blockchain: Blockchain,
                     prevBlockTimestamp: Option[Long],
                     micro: MicroBlock,
                     timestamp: Long): Either[ValidationError, Diff] = {
    for {
      // microblocks are processed within block which is next after 40-only-block which goes on top of activated height
      _ <- Either.cond(blockchain.activatedFeatures().contains(BlockchainFeatures.NG.id), (), ActivationError(s"MicroBlocks are not yet activated"))
      _ <- micro.signaturesValid()
      r <- apply(settings, blockchain, prevBlockTimestamp)(micro.sender, None, None, timestamp, micro.transactionData, 0)
    } yield r
  }

  private def apply(settings: FunctionalitySettings, blockchain: Blockchain, prevBlockTimestamp: Option[Long])(
      blockGenerator: Address,
      prevBlockFeeDistr: Option[Diff],
      currentBlockFeeDistr: Option[Diff],
      timestamp: Long,
      txs: Seq[Transaction],
      heightDiff: Int): Either[ValidationError, Diff] = {
    val currentBlockHeight = blockchain.height + heightDiff
    val txDiffer           = TransactionDiffer(settings, prevBlockTimestamp, timestamp, currentBlockHeight) _

    val txsDiffEi = currentBlockFeeDistr match {
      case Some(feedistr) =>
        txs.foldLeft(right(Monoid.combine(prevBlockFeeDistr.orEmpty, feedistr))) {
          case (ei, tx) =>
            ei.flatMap(
              diff =>
                txDiffer(composite(blockchain, diff), tx)
                  .map(newDiff => diff.combine(newDiff)))
        }
      case None =>
        txs.foldLeft(right(prevBlockFeeDistr.orEmpty)) {
          case (ei, tx) =>
            ei.flatMap(diff =>
              txDiffer(composite(blockchain, diff), tx).map { newDiff =>
                diff.combine(
                  newDiff.copy(
                    portfolios = newDiff.portfolios.combine(Map(blockGenerator -> tx.feeDiff()).mapValues(_.multiply(Block.CurrentBlockFeePart)))))
            })
        }
    }

    txsDiffEi.map { d =>
      val diffWithCancelledLeases =
        if (currentBlockHeight == settings.resetEffectiveBalancesAtHeight)
          Monoid.combine(d, CancelAllLeases(composite(blockchain, d)))
        else d

      val diffWithLeasePatches =
        if (currentBlockHeight == settings.blockVersion3AfterHeight)
          Monoid.combine(diffWithCancelledLeases, CancelLeaseOverflow(composite(blockchain, diffWithCancelledLeases)))
        else diffWithCancelledLeases

      val diffWithCancelledLeaseIns =
        if (blockchain.featureActivationHeight(BlockchainFeatures.DataTransaction.id).contains(currentBlockHeight))
          Monoid.combine(diffWithLeasePatches, CancelInvalidLeaseIn(composite(blockchain, diffWithLeasePatches)))
        else diffWithLeasePatches

      diffWithCancelledLeaseIns
    }
  }
}
