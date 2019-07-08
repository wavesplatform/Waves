package com.wavesplatform.state.diffs

import cats.implicits._
import cats.kernel.Monoid
import cats.syntax.either.catsSyntaxEitherId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.state._
import com.wavesplatform.state.patch.{CancelAllLeases, CancelInvalidLeaseIn, CancelLeaseOverflow}
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.{ActivationError, _}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utils.ScorexLogging

object BlockDiffer extends ScorexLogging {
  final case class DetailedDiff(parentDiff: Diff, transactionDiffs: Seq[Diff])
  final case class Result(diff: Diff, carry: Long, totalFee: Long, constraint: MiningConstraint, detailedDiff: DetailedDiff)

  def fromBlock(blockchain: Blockchain,
                maybePrevBlock: Option[Block],
                block: Block,
                constraint: MiningConstraint,
                verify: Boolean = true): Either[ValidationError, Result] =
    fromBlockTraced(blockchain, maybePrevBlock, block, constraint, verify).resultE

  def fromBlockTraced(blockchain: Blockchain,
                      maybePrevBlock: Option[Block],
                      block: Block,
                      constraint: MiningConstraint,
                      verify: Boolean = true): TracedResult[ValidationError, Result] = {
    val stateHeight = blockchain.height

    // height switch is next after activation
    val ngHeight          = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)
    val sponsorshipHeight = Sponsorship.sponsoredFeesSwitchHeight(blockchain)

    lazy val prevBlockFeeDistr: Option[Portfolio] =
      if (stateHeight >= sponsorshipHeight)
        Some(Portfolio.empty.copy(balance = blockchain.carryFee))
      else if (stateHeight > ngHeight)
        maybePrevBlock.map(_.prevBlockFeePart())
      else None

    lazy val currentBlockFeeDistr: Option[Portfolio] =
      if (stateHeight < ngHeight)
        Some(block.feesPortfolio())
      else
        None

    for {
      _ <- TracedResult(if (verify) block.signaturesValid() else Right(()))
      r <- apply(
        CompositeBlockchain(blockchain, newBlock = Some(block)),
        constraint,
        maybePrevBlock.map(_.timestamp),
        prevBlockFeeDistr,
        currentBlockFeeDistr,
        block.transactionData,
        verify
      )
    } yield r
  }

  def fromMicroBlock(blockchain: Blockchain,
                     prevBlockTimestamp: Option[Long],
                     micro: MicroBlock,
                     timestamp: Long,
                     constraint: MiningConstraint,
                     verify: Boolean = true): Either[ValidationError, Result] =
    fromMicroBlockTraced(blockchain, prevBlockTimestamp, micro, timestamp, constraint, verify).resultE

  def fromMicroBlockTraced(blockchain: Blockchain,
                           prevBlockTimestamp: Option[Long],
                           micro: MicroBlock,
                           timestamp: Long,
                           constraint: MiningConstraint,
                           verify: Boolean = true): TracedResult[ValidationError, Result] = {
    for {
      // microblocks are processed within block which is next after 40-only-block which goes on top of activated height
      _ <- TracedResult(
        Either.cond(
          blockchain.activatedFeatures.contains(BlockchainFeatures.NG.id),
          (),
          ActivationError(s"MicroBlocks are not yet activated")
        ))
      _ <- TracedResult(micro.signaturesValid())
      r <- apply(
        blockchain,
        constraint,
        prevBlockTimestamp,
        None,
        None,
        micro.transactionData,
        verify
      )
    } yield r
  }

  private[this] def apply(blockchain: Blockchain,
                          initConstraint: MiningConstraint,
                          prevBlockTimestamp: Option[Long],
                          prevBlockFeeDistr: Option[Portfolio],
                          currentBlockFeeDistr: Option[Portfolio],
                          txs: Seq[Transaction],
                          verify: Boolean): TracedResult[ValidationError, Result] = {
    def updateConstraint(constraint: MiningConstraint, blockchain: Blockchain, tx: Transaction, diff: Diff): MiningConstraint =
      constraint.put(blockchain, tx, diff)

    val currentBlockHeight = blockchain.height
    val timestamp          = blockchain.lastBlockTimestamp.get
    val lastBlock          = blockchain.lastBlock.get
    val blockGenerator     = lastBlock.sender.toAddress

    val txDiffer       = TransactionDiffer(prevBlockTimestamp, timestamp, verify) _
    val initDiff       = Diff.empty.copy(portfolios = Map(blockGenerator -> currentBlockFeeDistr.orElse(prevBlockFeeDistr).orEmpty))
    val hasNg          = currentBlockFeeDistr.isEmpty
    val hasSponsorship = currentBlockHeight >= Sponsorship.sponsoredFeesSwitchHeight(blockchain)

    def clearSponsorship(blockchain: Blockchain, portfolio: Portfolio): (Portfolio, Long) = {
      val spPf =
        if (hasSponsorship)
          Portfolio.empty.copy(
            balance = portfolio.balance +
              portfolio.assets.map {
                case (assetId, assetFee) =>
                  val baseFee = blockchain.assetDescription(assetId).get.sponsorship
                  Sponsorship.toWaves(assetFee, baseFee)
              }.sum)
        else portfolio

      val ngPf = if (hasNg) {
        val curPf  = spPf.multiply(Block.CurrentBlockFeePart)
        val nextPf = spPf.minus(curPf)
        (curPf, nextPf.balance)
      } else (spPf, 0L)
      if (hasSponsorship) ngPf else ngPf.copy(_2 = 0L)
    }

    txs
      .foldLeft(TracedResult(Result(initDiff, 0L, 0L, initConstraint, DetailedDiff(initDiff, Seq.empty)).asRight[ValidationError])) {
        case (acc @ TracedResult(Left(_), _), _) => acc
        case (TracedResult(Right(Result(currDiff, carryFee, currTotalFee, currConstraint, DetailedDiff(parentDiff, txDiffs))), _), tx) =>
          val currBlockchain = CompositeBlockchain(blockchain, Some(currDiff))
          txDiffer(currBlockchain, tx).flatMap { newDiff =>
            val updatedConstraint = updateConstraint(currConstraint, currBlockchain, tx, newDiff)
            if (updatedConstraint.isOverfilled)
              TracedResult(Left(GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint")))
            else {
              val (curBlockFees, nextBlockFee) = clearSponsorship(currBlockchain, tx.feeDiff())
              val totalWavesFee                = currTotalFee + curBlockFees.balance + nextBlockFee

              val (resultDiff, resultCarryFee, minerDiff) =
                if (hasNg) {
                  val minerDiff = Diff.empty.copy(portfolios = Map(blockGenerator -> curBlockFees))
                  (newDiff.combine(minerDiff), carryFee + nextBlockFee, minerDiff)
                } else (newDiff, 0L, Diff.empty)

              Right(
                Result(currDiff.combine(resultDiff),
                       resultCarryFee,
                       totalWavesFee,
                       updatedConstraint,
                       DetailedDiff(parentDiff.combine(minerDiff), txDiffs :+ newDiff)))
            }
          }
      }
      .map { result =>
        final case class Patch(predicate: Blockchain => Boolean, patch: Blockchain => Diff)
        def applyAll(patches: Patch*) = patches.foldLeft((result.diff, result.detailedDiff.parentDiff)) {
          case (r @ (previousDiff, previousPatchDiff), p) =>
            val currentBlockchain = CompositeBlockchain(blockchain, Some(previousDiff))
            if (p.predicate(currentBlockchain)) {
              val patchDiff = p.patch(currentBlockchain)
              (Monoid.combine(previousDiff, patchDiff), Monoid.combine(previousPatchDiff, patchDiff))
            } else {
              r
            }
        }

        val (diffWithPatches, patchDiff) = applyAll(
          Patch(
            _ => currentBlockHeight == blockchain.settings.functionalitySettings.resetEffectiveBalancesAtHeight,
            CancelAllLeases(_)
          ),
          Patch(
            _ => currentBlockHeight == blockchain.settings.functionalitySettings.blockVersion3AfterHeight,
            CancelLeaseOverflow(_)
          ),
          Patch(
            _.featureActivationHeight(BlockchainFeatures.DataTransaction.id).contains(currentBlockHeight),
            CancelInvalidLeaseIn(_)
          )
        )
        result.copy(diff = diffWithPatches, detailedDiff = result.detailedDiff.copy(parentDiff = patchDiff))
      }
  }
}
