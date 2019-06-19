package com.wavesplatform.state.diffs

import cats.implicits._
import cats.syntax.either.catsSyntaxEitherId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.state._
import com.wavesplatform.state.patch.{CancelAllLeases, CancelInvalidLeaseIn, CancelLeaseOverflow}
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.reader.CompositeBlockchain.composite
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.{ActivationError, _}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
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

    // Fixes lastBlockInfo() in scripts issue
    val blockchainWithLastBlock = CompositeBlockchain.withLastBlock(blockchain, block)

    for {
      _ <- TracedResult(if (verify) block.signaturesValid() else Right(()))
      r <- apply(
        blockchainWithLastBlock,
        constraint,
        maybePrevBlock.map(_.timestamp),
        prevBlockFeeDistr,
        currentBlockFeeDistr,
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
        None,
        None,
        micro.transactionData,
        verify
      )
    } yield r
  }

  private[this] def apply[Constraint <: MiningConstraint](blockchain: Blockchain,
                                                          initConstraint: Constraint,
                                                          prevBlockTimestamp: Option[Long],
                                                          prevBlockFeeDistr: Option[Portfolio],
                                                          currentBlockFeeDistr: Option[Portfolio],
                                                          txs: Seq[Transaction],
                                                          verify: Boolean): TracedResult[ValidationError, Result[Constraint]] = {
    def updateConstraint(constraint: Constraint, blockchain: Blockchain, tx: Transaction, diff: Diff): Constraint =
      constraint.put(blockchain, tx, diff).asInstanceOf[Constraint]

    val currentBlockHeight = blockchain.height
    val timestamp          = blockchain.lastBlockTimestamp.get
    val lastBlock          = blockchain.lastBlock.get
    val blockGenerator     = lastBlock.sender.toAddress

    val txDiffer       = TransactionDiffer(prevBlockTimestamp, timestamp, currentBlockHeight, verify) _
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
      .foldLeft(TracedResult((composite(blockchain, initDiff), Result(initDiff, 0L, 0L, initConstraint)).asRight[ValidationError])) {
        case (acc @ TracedResult(Left(_), _), _) => acc
        case (TracedResult(Right((blockchain, Result(currDiff, carryFee, currTotalFee, currConstraint))), _), tx) =>
          txDiffer(blockchain, tx).flatMap { newDiff =>
            val updatedConstraint = updateConstraint(currConstraint, blockchain, tx, newDiff)
            if (updatedConstraint.isOverfilled)
              TracedResult(Left(GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint")))
            else {
              val (curBlockFees, nextBlockFee) = clearSponsorship(blockchain, tx.feeDiff())
              val totalWavesFee                = currTotalFee + curBlockFees.balance + nextBlockFee

              val (resultDiff, resultCarryFee) =
                if (hasNg)
                  (newDiff.combine(Diff.empty.copy(portfolios = Map(blockGenerator -> curBlockFees))), carryFee + nextBlockFee)
                else
                  (newDiff, 0L)

              Right((composite(blockchain, resultDiff), Result(currDiff.combine(resultDiff), resultCarryFee, totalWavesFee, updatedConstraint)))
            }
          }
      }
      .map {
        case (blockchain, result) =>
          final case class Patch(predicate: CompositeBlockchain => Boolean, patch: CompositeBlockchain => Diff)
          def applyAll(patches: Patch*) = patches.foldLeft((blockchain, Diff.empty)) {
            case ((blockchain, cd), p) =>
              if (p.predicate(blockchain)) {
                val pd = p.patch(blockchain)
                (composite(blockchain, pd), cd.combine(pd))
              } else {
                (blockchain, cd)
              }
          }

          val (_, patchDiff) = applyAll(
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

          result.copy(diff = result.diff.combine(patchDiff))
      }
  }
}
