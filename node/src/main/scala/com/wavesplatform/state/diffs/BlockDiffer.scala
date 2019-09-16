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
import com.wavesplatform.transaction.TxValidationError.{ActivationError, _}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils.ScorexLogging

//noinspection VariablePatternShadow
object BlockDiffer extends ScorexLogging {
  final case class Result[Constraint <: MiningConstraint](diff: Diff, carry: Long, totalFee: Long, constraint: Constraint)
  type GenResult = Result[MiningConstraint]

  def fromBlock[Constraint <: MiningConstraint](blockchain: Blockchain,
                                                maybePrevBlock: Option[Block],
                                                block: Block,
                                                constraint: Constraint,
                                                verify: Boolean = true): Either[ValidationError, Result[Constraint]] =
    fromBlockTraced(blockchain, maybePrevBlock, block, constraint, verify).resultE

  def fromBlockTraced[Constraint <: MiningConstraint](blockchain: Blockchain,
                                                      maybePrevBlock: Option[Block],
                                                      block: Block,
                                                      constraint: Constraint,
                                                      verify: Boolean = true): TracedResult[ValidationError, Result[Constraint]] = {
    val stateHeight = blockchain.height

    // height switch is next after activation
    val ngHeight          = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)
    val sponsorshipHeight = Sponsorship.sponsoredFeesSwitchHeight(blockchain)

    val minerRewardDistr: Portfolio =
        blockchain.lastBlockReward.map(Portfolio.build(Asset.Waves, _)).getOrElse(Portfolio.empty)

    val prevBlockFeeDistr: Portfolio =
      if (stateHeight >= sponsorshipHeight)
        Portfolio.empty.copy(balance = blockchain.carryFee)
      else if (stateHeight > ngHeight)
        maybePrevBlock.map(_.prevBlockFeePart()).getOrElse(Portfolio.empty)
      else Portfolio.empty

    val currentBlockFeeDistr: Portfolio =
      if (stateHeight < ngHeight)
        block.feesPortfolio()
      else
        Portfolio.empty

    val initDiff = Diff.empty.copy(portfolios = Map(block.sender.toAddress -> (minerRewardDistr |+| currentBlockFeeDistr |+| prevBlockFeeDistr)))

    for {
      _ <- TracedResult(if (verify) block.signaturesValid() else Right(()))
      r <- apply(
        CompositeBlockchain(blockchain, newBlock = Some(block)),
        constraint,
        maybePrevBlock.map(_.timestamp),
        initDiff,
        stateHeight >= ngHeight,
        block.transactionData,
        verify
      )
    } yield r
  }

  def fromMicroBlock[Constraint <: MiningConstraint](blockchain: Blockchain,
                                                     prevBlockTimestamp: Option[Long],
                                                     micro: MicroBlock,
                                                     timestamp: Long,
                                                     constraint: Constraint,
                                                     verify: Boolean = true): Either[ValidationError, Result[Constraint]] =
    fromMicroBlockTraced(blockchain, prevBlockTimestamp, micro, timestamp, constraint, verify).resultE

  def fromMicroBlockTraced[Constraint <: MiningConstraint](blockchain: Blockchain,
                                                           prevBlockTimestamp: Option[Long],
                                                           micro: MicroBlock,
                                                           timestamp: Long,
                                                           constraint: Constraint,
                                                           verify: Boolean = true): TracedResult[ValidationError, Result[Constraint]] = {
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
        Diff.empty,
        true,
        micro.transactionData,
        verify
      )
    } yield r
  }

  private[this] def apply[Constraint <: MiningConstraint](blockchain: Blockchain,
                                                          initConstraint: Constraint,
                                                          prevBlockTimestamp: Option[Long],
                                                          initDiff: Diff,
                                                          hasNg: Boolean,
                                                          txs: Seq[Transaction],
                                                          verify: Boolean): TracedResult[ValidationError, Result[Constraint]] = {
    def updateConstraint(constraint: Constraint, blockchain: Blockchain, tx: Transaction, diff: Diff): Constraint =
      constraint.put(blockchain, tx, diff).asInstanceOf[Constraint]

    val currentBlockHeight = blockchain.height
    val timestamp          = blockchain.lastBlockTimestamp.get
    val lastBlock          = blockchain.lastBlock.get
    val blockGenerator     = lastBlock.sender.toAddress

    val txDiffer       = TransactionDiffer(prevBlockTimestamp, timestamp, currentBlockHeight, verify) _
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
      .foldLeft(TracedResult(Result(initDiff, 0L, 0L, initConstraint).asRight[ValidationError])) {
        case (acc @ TracedResult(Left(_), _), _) => acc
        case (TracedResult(Right(Result(currDiff, carryFee, currTotalFee, currConstraint)), _), tx) =>
          val currBlockchain = CompositeBlockchain(blockchain, Some(currDiff))
          txDiffer(currBlockchain, tx).flatMap { newDiff =>
            val updatedConstraint = updateConstraint(currConstraint, currBlockchain, tx, newDiff)
            if (updatedConstraint.isOverfilled)
              TracedResult(Left(GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint")))
            else {
              val (curBlockFees, nextBlockFee) = clearSponsorship(currBlockchain, tx.feeDiff())
              val totalWavesFee                = currTotalFee + curBlockFees.balance + nextBlockFee

              val (resultDiff, resultCarryFee) =
                if (hasNg)
                  (newDiff.combine(Diff.empty.copy(portfolios = Map(blockGenerator -> curBlockFees))), carryFee + nextBlockFee)
                else
                  (newDiff, 0L)

              Right(Result(currDiff.combine(resultDiff), resultCarryFee, totalWavesFee, updatedConstraint))
            }
          }
      }
      .map { result =>
        final case class Patch(predicate: Blockchain => Boolean, patch: Blockchain => Diff)
        def applyAll(patches: Patch*) = patches.foldLeft(result.diff) {
          case (previousDiff, p) =>
            val currentBlockchain = CompositeBlockchain(blockchain, Some(previousDiff))
            if (p.predicate(currentBlockchain)) {
              val patchDiff = p.patch(currentBlockchain)
              Monoid.combine(previousDiff, patchDiff)
            } else {
              previousDiff
            }
        }

        val patchDiff = applyAll(
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
        result.copy(diff = patchDiff)
      }
  }
}
