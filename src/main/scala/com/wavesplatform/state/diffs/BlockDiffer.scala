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

  private def clearSponsorship(blockchain: Blockchain, portfolio: Portfolio, height: Int, fs: FunctionalitySettings): Portfolio = {
    if (height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain, fs)) {
      val sponsoredAssets = portfolio.assets
        .map {
          case (assetId, totalFee) =>
            (assetId, totalFee, blockchain.assetDescription(assetId))
        }
        .collect {
          case (assetId, totalFee, Some(desc)) if desc.sponsorship > 0 =>
            (assetId, totalFee, desc.sponsorship)
        }
      val unsponsoredPf = portfolio.copy(assets = portfolio.assets -- sponsoredAssets.map(_._1))
      val sponsoredWaves = sponsoredAssets.map {
        case (_, totalFee, baseFee) => Sponsorship.toWaves(totalFee, baseFee)
      }.sum
      unsponsoredPf.copy(balance = unsponsoredPf.balance + sponsoredWaves)
    } else portfolio
  }

  def fromBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                blockchain: Blockchain,
                                                maybePrevBlock: Option[Block],
                                                block: Block,
                                                constraint: Constraint): Either[ValidationError, (Diff, Constraint)] = {
    val blockSigner = block.signerData.generator.toAddress
    val stateHeight = blockchain.height

    // height switch is next after activation
    val ng4060switchHeight = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)

    lazy val prevBlockFeeDistr: Option[Diff] =
      if (stateHeight > ng4060switchHeight)
        maybePrevBlock.map(
          prevBlock =>
            Diff.empty.copy(portfolios = Map(blockSigner ->
              clearSponsorship(blockchain, prevBlock.prevBlockFeePart(), stateHeight, settings))))
      else None

    lazy val currentBlockFeeDistr =
      if (stateHeight < ng4060switchHeight)
        Some(Diff.empty.copy(portfolios = Map(blockSigner -> block.feesPortfolio())))
      else
        None

    val prevBlockTimestamp = maybePrevBlock.map(_.timestamp)
    for {
      _ <- block.signaturesValid()
      r <- apply(
        settings,
        blockchain,
        constraint,
        prevBlockTimestamp,
        block.signerData.generator,
        prevBlockFeeDistr,
        currentBlockFeeDistr,
        block.timestamp,
        block.transactionData,
        1
      )
    } yield r
  }

  def fromMicroBlock[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                     blockchain: Blockchain,
                                                     prevBlockTimestamp: Option[Long],
                                                     micro: MicroBlock,
                                                     timestamp: Long,
                                                     constraint: Constraint): Either[ValidationError, (Diff, Constraint)] = {
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
        0
      )
    } yield r
  }

  private def apply[Constraint <: MiningConstraint](settings: FunctionalitySettings,
                                                    blockchain: Blockchain,
                                                    initConstraint: Constraint,
                                                    prevBlockTimestamp: Option[Long],
                                                    blockGenerator: Address,
                                                    prevBlockFeeDistr: Option[Diff],
                                                    currentBlockFeeDistr: Option[Diff],
                                                    timestamp: Long,
                                                    txs: Seq[Transaction],
                                                    heightDiff: Int): Either[ValidationError, (Diff, Constraint)] = {
    def updateConstraint(constraint: Constraint, blockchain: Blockchain, tx: Transaction): Constraint =
      constraint.put(blockchain, tx).asInstanceOf[Constraint]

    val currentBlockHeight = blockchain.height + heightDiff
    val txDiffer           = TransactionDiffer(settings, prevBlockTimestamp, timestamp, currentBlockHeight) _

    val txsDiffEi = currentBlockFeeDistr match {
      case Some(feedistr) =>
        val initDiff = Monoid.combine(prevBlockFeeDistr.orEmpty, feedistr)
        txs.foldLeft((initDiff, initConstraint).asRight[ValidationError]) {
          case (r @ Left(_), _) => r
          case (Right((currDiff, currConstraint)), tx) =>
            val updatedBlockchain = composite(blockchain, currDiff)
            val updatedConstraint = updateConstraint(currConstraint, updatedBlockchain, tx)
            if (updatedConstraint.isOverfilled) Left(ValidationError.GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint"))
            else
              txDiffer(updatedBlockchain, tx).map { newDiff =>
                (currDiff.combine(newDiff), updatedConstraint)
              }
        }
      case None =>
        txs.foldLeft((prevBlockFeeDistr.orEmpty, initConstraint).asRight[ValidationError]) {
          case (r @ Left(_), _) => r
          case (Right((currDiff, currConstraint)), tx) =>
            val updatedBlockchain = composite(blockchain, currDiff)
            val updatedConstraint = updateConstraint(currConstraint, updatedBlockchain, tx)
            if (updatedConstraint.isOverfilled) Left(ValidationError.GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint"))
            else
              txDiffer(updatedBlockchain, tx).map { newDiff =>
                val updatedPortfolios = newDiff.portfolios.combine(
                  Map(blockGenerator -> clearSponsorship(blockchain, tx.feeDiff().multiply(Block.CurrentBlockFeePart), currentBlockHeight, settings))
                )

                (currDiff.combine(newDiff.copy(portfolios = updatedPortfolios)), updatedConstraint)
              }
        }
    }

    txsDiffEi.map {
      case (d, constraint) =>
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

        (diffWithCancelledLeaseIns, constraint)
    }
  }
}
