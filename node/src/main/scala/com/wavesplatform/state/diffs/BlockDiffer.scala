package com.wavesplatform.state.diffs

import cats.Monoid
import cats.implicits._
import cats.syntax.either.catsSyntaxEitherId
import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.state.patch.{CancelAllLeases, CancelInvalidLeaseIn, CancelLeaseOverflow}
import com.wavesplatform.state.reader.CompositeBlockchain.composite
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.TxValidationError.ActivationError
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.transaction.TxValidationError._

object BlockDiffer extends ScorexLogging {
  final case class Result[Constraint <: MiningConstraint](diff: Diff, carry: Long, totalFee: Long, constraint: Constraint)
  type GenResult = Result[MiningConstraint]

  def fromBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                blockchain: Blockchain,
                                                maybePrevBlock: Option[Block],
                                                block: Block,
                                                constraint: Constraint,
                                                verify: Boolean = true): Either[ValidationError, Result[Constraint]] =
    fromBlockTraced(settings, blockchain, maybePrevBlock, block, constraint, verify).resultE

  def fromBlockTraced[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                      blockchain: Blockchain,
                                                      maybePrevBlock: Option[Block],
                                                      block: Block,
                                                      constraint: Constraint,
                                                      verify: Boolean = true): TracedResult[ValidationError, Result[Constraint]] = {
    val stateHeight = blockchain.height

    // height switch is next after activation
    val ngHeight          = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)
    val sponsorshipHeight = Sponsorship.sponsoredFeesSwitchHeight(blockchain, settings)

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
      _ <- TracedResult(block.signaturesValid())
      r <- apply(
        settings,
        blockchain,
        constraint,
        maybePrevBlock.map(_.timestamp),
        block.signerData.generator,
        prevBlockFeeDistr,
        currentBlockFeeDistr,
        block.timestamp,
        block.transactionData,
        stateHeight + 1,
        verify
      )
    } yield r
  }

  def fromMicroBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                blockchain: Blockchain,
                                                prevBlockTimestamp: Option[Long],
                                                micro: MicroBlock,
                                                timestamp: Long,
                                                constraint: Constraint,
                                                verify: Boolean = true): Either[ValidationError, Result[Constraint]] =
    fromMicroBlockTraced(settings, blockchain, prevBlockTimestamp, micro, timestamp, constraint, verify).resultE

  def fromMicroBlockTraced[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                     blockchain: Blockchain,
                                                     prevBlockTimestamp: Option[Long],
                                                     micro: MicroBlock,
                                                     timestamp: Long,
                                                     constraint: Constraint,
                                                     verify: Boolean = true): TracedResult[ValidationError, Result[Constraint]] = {
    for {
      // microblocks are processed within block which is next after 40-only-block which goes on top of activated height
      _ <- TracedResult(Either.cond(
        blockchain.activatedFeatures.contains(BlockchainFeatures.NG.id),
        (),
        ActivationError(s"MicroBlocks are not yet activated")
      ))
      _ <- TracedResult(micro.signaturesValid())
      r <- apply(
        settings,
        blockchain,
        constraint,
        prevBlockTimestamp,
        micro.sender,
        None,
        None,
        timestamp,
        micro.transactionData,
        blockchain.height,
        verify
      )
    } yield r
  }

  private def apply[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                    blockchain: Blockchain,
                                                    initConstraint: Constraint,
                                                    prevBlockTimestamp: Option[Long],
                                                    blockGenerator: Address,
                                                    prevBlockFeeDistr: Option[Portfolio],
                                                    currentBlockFeeDistr: Option[Portfolio],
                                                    timestamp: Long,
                                                    txs: Seq[Transaction],
                                                    currentBlockHeight: Int,
                                                    verify: Boolean): TracedResult[ValidationError, Result[Constraint]] = {
    def updateConstraint(constraint: Constraint, blockchain: Blockchain, tx: Transaction, diff: Diff): Constraint =
      constraint.put(blockchain, tx, diff).asInstanceOf[Constraint]

    val txDiffer       = TransactionDiffer(settings, prevBlockTimestamp, timestamp, currentBlockHeight, verify) _
    val initDiff       = Diff.empty.copy(portfolios = Map(blockGenerator -> currentBlockFeeDistr.orElse(prevBlockFeeDistr).orEmpty))
    val hasNg          = currentBlockFeeDistr.isEmpty
    val hasSponsorship = currentBlockHeight >= Sponsorship.sponsoredFeesSwitchHeight(blockchain, settings)

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
          val updatedBlockchain = composite(blockchain, currDiff)
          txDiffer(updatedBlockchain, tx).flatMap { newDiff =>
            val updatedConstraint = updateConstraint(currConstraint, updatedBlockchain, tx, newDiff)
            if (updatedConstraint.isOverfilled)
              TracedResult(Left(GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint")))
            else {
              val updatedDiff = currDiff.combine(newDiff)

              val (curBlockFees, nextBlockFee) = clearSponsorship(updatedBlockchain, tx.feeDiff())
              val totalWavesFee                = currTotalFee + curBlockFees.balance + nextBlockFee

              Right(
                if (hasNg) {
                  val diff = updatedDiff.combine(Diff.empty.copy(portfolios = Map(blockGenerator -> curBlockFees)))
                  Result(diff, carryFee + nextBlockFee, totalWavesFee, updatedConstraint)
                } else {
                  Result(updatedDiff, 0L, totalWavesFee, updatedConstraint)
                }
              )
            }
          }
      }
      .map {
        case Result(diff, carry, totalFee, constraint) =>
          val diffWithCancelledLeases =
            if (currentBlockHeight == settings.resetEffectiveBalancesAtHeight)
              Monoid.combine(diff, CancelAllLeases(composite(blockchain, diff)))
            else diff

          val diffWithLeasePatches =
            if (currentBlockHeight == settings.blockVersion3AfterHeight)
              Monoid.combine(diffWithCancelledLeases, CancelLeaseOverflow(composite(blockchain, diffWithCancelledLeases)))
            else diffWithCancelledLeases

          val diffWithCancelledLeaseIns =
            if (blockchain.featureActivationHeight(BlockchainFeatures.DataTransaction.id).contains(currentBlockHeight))
              Monoid.combine(diffWithLeasePatches, CancelInvalidLeaseIn(composite(blockchain, diffWithLeasePatches)))
            else diffWithLeasePatches

          Result(diffWithCancelledLeaseIns, carry, totalFee, constraint)
      }
  }
}
