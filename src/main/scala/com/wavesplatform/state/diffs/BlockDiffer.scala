package com.wavesplatform.state.diffs

import cats.Monoid
import cats.implicits._
import cats.syntax.either.catsSyntaxEitherId
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.state.patch.{CancelAllLeases, CancelInvalidLeaseIn, CancelLeaseOverflow}
import com.wavesplatform.state.reader.CompositeBlockchain.composite
import com.wavesplatform.account.Address
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.transaction.ValidationError.ActivationError
import com.wavesplatform.transaction.{Transaction, ValidationError}

object BlockDiffer extends ScorexLogging with Instrumented {

  def fromBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                blockchain: Blockchain,
                                                maybePrevBlock: Option[Block],
                                                block: Block,
                                                constraint: Constraint,
                                                verify: Boolean = true): Either[ValidationError, (Diff, Long, Constraint)] = {
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
      _ <- block.signaturesValid()
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
                                                     verify: Boolean = true): Either[ValidationError, (Diff, Long, Constraint)] = {
    for {
      // microblocks are processed within block which is next after 40-only-block which goes on top of activated height
      _ <- Either.cond(blockchain.activatedFeatures.contains(BlockchainFeatures.NG.id), (), ActivationError(s"MicroBlocks are not yet activated"))
      _ <- micro.signaturesValid()
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
                                                    verify: Boolean): Either[ValidationError, (Diff, Long, Constraint)] = {
    def updateConstraint(constraint: Constraint, blockchain: Blockchain, tx: Transaction): Constraint =
      constraint.put(blockchain, tx).asInstanceOf[Constraint]

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
      .foldLeft((initDiff, 0L, initConstraint).asRight[ValidationError]) {
        case (r @ Left(_), _) => r
        case (Right((currDiff, carryFee, currConstraint)), tx) =>
          val updatedBlockchain = composite(blockchain, currDiff)
          val updatedConstraint = updateConstraint(currConstraint, updatedBlockchain, tx)
          if (updatedConstraint.isOverfilled)
            Left(ValidationError.GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint"))
          else
            txDiffer(updatedBlockchain, tx).map { newDiff =>
              val updatedDiff = currDiff.combine(newDiff)
              if (hasNg) {
                val (curBlockFees, nextBlockFee) = clearSponsorship(updatedBlockchain, tx.feeDiff())
                val diff                         = updatedDiff.combine(Diff.empty.copy(portfolios = Map(blockGenerator -> curBlockFees)))
                (diff, carryFee + nextBlockFee, updatedConstraint)
              } else (updatedDiff, 0L, updatedConstraint)
            }
      }
      .map {
        case (d, carry, constraint) =>
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

          (diffWithCancelledLeaseIns, carry, constraint)
      }
  }
}
