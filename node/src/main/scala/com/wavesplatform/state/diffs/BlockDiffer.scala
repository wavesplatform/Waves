package com.wavesplatform.state.diffs

import cats.implicits._
import cats.kernel.Monoid
import cats.syntax.either.catsSyntaxEitherId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.state._
import com.wavesplatform.state.patch._
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{ActivationError, _}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils.ScorexLogging

object BlockDiffer extends ScorexLogging {
  final case class DetailedDiff(parentDiff: Diff, transactionDiffs: List[Diff])
  final case class Result(diff: Diff, carry: Long, totalFee: Long, constraint: MiningConstraint, detailedDiff: DetailedDiff)

  case class Fraction(dividend: Int, divider: Int) {
    def apply(l: Long): Long = l / divider * dividend
  }

  val CurrentBlockFeePart: Fraction = Fraction(2, 5)

  def fromBlock(
      blockchain: Blockchain,
      maybePrevBlock: Option[Block],
      block: Block,
      constraint: MiningConstraint,
      hitSource: ByteStr,
      verify: Boolean = true
  ): Either[ValidationError, Result] =
    fromBlockTraced(blockchain, maybePrevBlock, block, constraint, hitSource, verify).resultE

  def fromBlockTraced(
      blockchain: Blockchain,
      maybePrevBlock: Option[Block],
      block: Block,
      constraint: MiningConstraint,
      hitSource: ByteStr,
      verify: Boolean
  ): TracedResult[ValidationError, Result] = {
    val stateHeight = blockchain.height

    // height switch is next after activation
    val ngHeight          = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)
    val sponsorshipHeight = Sponsorship.sponsoredFeesSwitchHeight(blockchain)

    val minerReward = blockchain.lastBlockReward.fold(Portfolio.empty)(Portfolio.waves)

    val feeFromPreviousBlock: Portfolio =
      if (stateHeight >= sponsorshipHeight) {
        Portfolio(balance = blockchain.carryFee)
      } else if (stateHeight > ngHeight) maybePrevBlock.fold(Portfolio.empty) { pb =>
        // it's important to combine tx fee fractions (instead of getting a fraction of the combined tx fee)
        // so that we end up with the same value as when computing per-transaction fee part
        // during microblock processing below
        Monoid.combineAll(pb.transactionData.map { t =>
          val pf = Portfolio.build(t.assetFee)
          pf.minus(pf.multiply(CurrentBlockFeePart))
        })
      } else Portfolio.empty

    val initialFeeFromThisBlock: Portfolio =
      if (stateHeight < ngHeight) {
        // before NG activation, miner immediately received all the fee from the block
        Monoid.combineAll(block.transactionData.map(_.assetFee).map(Portfolio.build))
      } else
        Portfolio.empty

    for {
      _ <- TracedResult(Either.cond(!verify || block.signatureValid(), (), GenericError(s"Block $block has invalid signature")))
      r <- apply(
        CompositeBlockchain(blockchain, Diff.empty, block, hitSource, 0, None),
        constraint,
        maybePrevBlock.map(_.header.timestamp),
        Diff.empty.copy(portfolios = Map(block.sender.toAddress -> (minerReward |+| initialFeeFromThisBlock |+| feeFromPreviousBlock))),
        stateHeight >= ngHeight,
        block.transactionData,
        verify
      )
    } yield r
  }

  def fromMicroBlock(
      blockchain: Blockchain,
      prevBlockTimestamp: Option[Long],
      micro: MicroBlock,
      timestamp: Long,
      constraint: MiningConstraint,
      verify: Boolean = true
  ): Either[ValidationError, Result] =
    fromMicroBlockTraced(blockchain, prevBlockTimestamp, micro, timestamp, constraint, verify).resultE

  def fromMicroBlockTraced(
      blockchain: Blockchain,
      prevBlockTimestamp: Option[Long],
      micro: MicroBlock,
      timestamp: Long,
      constraint: MiningConstraint,
      verify: Boolean = true
  ): TracedResult[ValidationError, Result] = {
    for {
      // microblocks are processed within block which is next after 40-only-block which goes on top of activated height
      _ <- TracedResult(
        Either.cond(
          blockchain.activatedFeatures.contains(BlockchainFeatures.NG.id),
          (),
          ActivationError(s"MicroBlocks are not yet activated")
        )
      )
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

  private def maybeApplySponsorship(blockchain: Blockchain, sponsorshipEnabled: Boolean, transactionFee: (Asset, Long)): (Asset, Long) =
    transactionFee match {
      case (ia: IssuedAsset, fee) if sponsorshipEnabled =>
        Waves -> Sponsorship.toWaves(fee, blockchain.assetDescription(ia).get.sponsorship)
      case _ => transactionFee
    }

  private[this] def apply(
      blockchain: Blockchain,
      initConstraint: MiningConstraint,
      prevBlockTimestamp: Option[Long],
      initDiff: Diff,
      hasNg: Boolean,
      txs: Seq[Transaction],
      verify: Boolean
  ): TracedResult[ValidationError, Result] = {
    def updateConstraint(constraint: MiningConstraint, blockchain: Blockchain, tx: Transaction, diff: Diff): MiningConstraint =
      constraint.put(blockchain, tx, diff)

    val currentBlockHeight = blockchain.height
    val timestamp          = blockchain.lastBlockTimestamp.get
    val blockGenerator     = blockchain.lastBlockHeader.get.header.generator.toAddress

    val txDiffer       = TransactionDiffer(prevBlockTimestamp, timestamp, verify) _
    val hasSponsorship = currentBlockHeight >= Sponsorship.sponsoredFeesSwitchHeight(blockchain)

    txs
      .foldLeft(TracedResult(Result(initDiff, 0L, 0L, initConstraint, DetailedDiff(initDiff, Nil)).asRight[ValidationError])) {
        case (acc @ TracedResult(Left(_), _), _) => acc
        case (TracedResult(Right(Result(currDiff, carryFee, currTotalFee, currConstraint, DetailedDiff(parentDiff, txDiffs))), _), tx) =>
          val currBlockchain = CompositeBlockchain(blockchain, currDiff)
          txDiffer(currBlockchain, tx).flatMap { thisTxDiff =>
            val updatedConstraint = updateConstraint(currConstraint, currBlockchain, tx, thisTxDiff)
            if (updatedConstraint.isOverfilled)
              TracedResult(Left(GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint")))
            else {
              val (feeAsset, feeAmount) = maybeApplySponsorship(currBlockchain, hasSponsorship, tx.assetFee)
              val currentBlockFee       = CurrentBlockFeePart(feeAmount)

              // unless NG is activated, miner has already received all the fee from this block by the time the first
              // transaction is processed (see abode), so there's no need to include tx fee into portfolio.
              // if NG is activated, just give them their 40%
              val minerPortfolio = if (!hasNg) Portfolio.empty else Portfolio.build(feeAsset, feeAmount).multiply(CurrentBlockFeePart)

              // carry is 60% of waves fees the next miner will get. obviously carry fee only makes sense when both
              // NG and sponsorship is active. also if sponsorship is active, feeAsset can only be Waves
              val carry = if (hasNg && hasSponsorship) feeAmount - currentBlockFee else 0

              val totalWavesFee = currTotalFee + (if (feeAsset == Waves) feeAmount else 0L)
              val minerDiff     = Diff.empty.copy(portfolios = Map(blockGenerator -> minerPortfolio))

              Right(
                Result(
                  currDiff |+| thisTxDiff |+| minerDiff,
                  carryFee + carry,
                  totalWavesFee,
                  updatedConstraint,
                  DetailedDiff(parentDiff.combine(minerDiff), thisTxDiff :: txDiffs)
                )
              )
            }
          }
      }
      .map { result =>
        def applyAll(patches: DiffPatchFactory*): (Diff, Diff) = patches.foldLeft((result.diff, result.detailedDiff.parentDiff)) {
          case (prevResult @ (previousDiff, previousPatchDiff), p) =>
            if (p.isApplicable(blockchain)) {
              val patchDiff = p()
              (Monoid.combine(previousDiff, patchDiff), Monoid.combine(previousPatchDiff, patchDiff))
            } else prevResult
        }

        val (diffWithPatches, patchDiff) = applyAll(CancelAllLeases, CancelLeaseOverflow, CancelInvalidLeaseIn)
        result.copy(diff = diffWithPatches, detailedDiff = result.detailedDiff.copy(parentDiff = patchDiff))
      }
  }
}
