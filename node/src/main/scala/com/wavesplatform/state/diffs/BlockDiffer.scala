package com.wavesplatform.state.diffs

import cats.implicits.{toBifunctorOps, toFoldableOps}
import cats.syntax.either.catsSyntaxEitherId
import cats.syntax.parallel.*
import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.state.*
import com.wavesplatform.state.patch.*
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{Asset, Authorized, GenesisTransaction, PaymentTransaction, ProvenTransaction, Transaction}
import com.wavesplatform.utils.Schedulers
import monix.eval.Task
import monix.execution.schedulers.SchedulerService

object BlockDiffer {
  implicit val sigverify: SchedulerService = Schedulers.fixedPool(4, "sigverify")

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
      loadCacheData: Seq[Address] => Unit = _ => (),
      verify: Boolean = true,
      enableExecutionLog: Boolean = false,
      txSignParCheck: Boolean = true
  ): Either[ValidationError, Result] =
    fromBlockTraced(blockchain, maybePrevBlock, block, constraint, hitSource, loadCacheData, verify, enableExecutionLog, txSignParCheck).resultE

  def fromBlockTraced(
      blockchain: Blockchain,
      maybePrevBlock: Option[Block],
      block: Block,
      constraint: MiningConstraint,
      hitSource: ByteStr,
      loadCacheData: Seq[Address] => Unit,
      verify: Boolean,
      enableExecutionLog: Boolean,
      txSignParCheck: Boolean
  ): TracedResult[ValidationError, Result] = {
    val stateHeight = blockchain.height

    // height switch is next after activation
    val ngHeight          = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)
    val sponsorshipHeight = Sponsorship.sponsoredFeesSwitchHeight(blockchain)

    val minerReward = blockchain.lastBlockReward.fold(Portfolio.empty)(Portfolio.waves)

    val feeFromPreviousBlockE =
      if (stateHeight >= sponsorshipHeight) {
        Right(Portfolio(balance = blockchain.carryFee))
      } else if (stateHeight > ngHeight) maybePrevBlock.fold(Portfolio.empty.asRight[String]) { pb =>
        // it's important to combine tx fee fractions (instead of getting a fraction of the combined tx fee)
        // so that we end up with the same value as when computing per-transaction fee part
        // during microblock processing below
        pb.transactionData
          .map { t =>
            val pf = Portfolio.build(t.assetFee)
            pf.minus(pf.multiply(CurrentBlockFeePart))
          }
          .foldM(Portfolio.empty)(_.combine(_))
      }
      else
        Right(Portfolio.empty)

    val initialFeeFromThisBlockE =
      if (stateHeight < ngHeight) {
        // before NG activation, miner immediately received all the fee from the block
        block.transactionData.map(_.assetFee).map(Portfolio.build).foldM(Portfolio.empty)(_.combine(_))
      } else
        Right(Portfolio.empty)

    val blockchainWithNewBlock = CompositeBlockchain(blockchain, Diff.empty, block, hitSource, 0, None)
    val initDiffE =
      for {
        feeFromPreviousBlock    <- feeFromPreviousBlockE
        initialFeeFromThisBlock <- initialFeeFromThisBlockE
        totalReward             <- minerReward.combine(initialFeeFromThisBlock).flatMap(_.combine(feeFromPreviousBlock))
        patches                 <- patchesDiff(blockchainWithNewBlock)
        resultDiff              <- Diff(portfolios = Map(block.sender.toAddress -> totalReward)).combineF(patches)
      } yield resultDiff

    for {
      _          <- TracedResult(Either.cond(!verify || block.signatureValid(), (), GenericError(s"Block $block has invalid signature")))
      resultDiff <- TracedResult(initDiffE.leftMap(GenericError(_)))
      r <- apply(
        blockchainWithNewBlock,
        constraint,
        maybePrevBlock.map(_.header.timestamp),
        resultDiff,
        stateHeight >= ngHeight,
        block.transactionData,
        loadCacheData,
        verify = verify,
        enableExecutionLog = enableExecutionLog,
        txSignParCheck = txSignParCheck
      )
    } yield r
  }

  def fromMicroBlock(
      blockchain: Blockchain,
      prevBlockTimestamp: Option[Long],
      micro: MicroBlock,
      constraint: MiningConstraint,
      loadCacheData: Seq[Address] => Unit = _ => (),
      verify: Boolean = true,
      enableExecutionLog: Boolean = false
  ): Either[ValidationError, Result] =
    fromMicroBlockTraced(blockchain, prevBlockTimestamp, micro, constraint, loadCacheData, verify, enableExecutionLog).resultE

  def fromMicroBlockTraced(
      blockchain: Blockchain,
      prevBlockTimestamp: Option[Long],
      micro: MicroBlock,
      constraint: MiningConstraint,
      loadCacheData: Seq[Address] => Unit = _ => (),
      verify: Boolean = true,
      enableExecutionLog: Boolean = false,
      txSignParCheck: Boolean = true
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
        hasNg = true,
        micro.transactionData,
        loadCacheData,
        verify = verify,
        enableExecutionLog = enableExecutionLog,
        txSignParCheck = txSignParCheck
      )
    } yield r
  }

  def maybeApplySponsorship(blockchain: Blockchain, sponsorshipEnabled: Boolean, transactionFee: (Asset, Long)): (Asset, Long) =
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
      loadCacheData: Seq[Address] => Unit,
      verify: Boolean,
      enableExecutionLog: Boolean,
      txSignParCheck: Boolean
  ): TracedResult[ValidationError, Result] = {
    def updateConstraint(constraint: MiningConstraint, blockchain: Blockchain, tx: Transaction, diff: Diff): MiningConstraint =
      constraint.put(blockchain, tx, diff)

    val currentBlockHeight = blockchain.height
    val timestamp          = blockchain.lastBlockTimestamp.get
    val blockGenerator     = blockchain.lastBlockHeader.get.header.generator.toAddress
    val rideV6Activated    = blockchain.isFeatureActivated(BlockchainFeatures.RideV6)

    val txDiffer       = TransactionDiffer(prevBlockTimestamp, timestamp, verify, enableExecutionLog = enableExecutionLog) _
    val hasSponsorship = currentBlockHeight >= Sponsorship.sponsoredFeesSwitchHeight(blockchain)

    if (verify && txSignParCheck) {
      txs
        .parUnorderedTraverse {
          case tx: ProvenTransaction =>
            Task {
              if (rideV6Activated) {
                tx.firstProofIsValidSignatureAfterV6
              } else {
                tx.firstProofIsValidSignatureBeforeV6
              }
            }.void
          case _ => Task.unit
        }
        .executeOn(sigverify)
        .runAsyncAndForget
    }

    prepareCaches(blockGenerator, txs, loadCacheData)

    txs
      .foldLeft(TracedResult(Result(initDiff, 0L, 0L, initConstraint, DetailedDiff(initDiff, Nil)).asRight[ValidationError])) {
        case (acc @ TracedResult(Left(_), _, _), _) => acc
        case (TracedResult(Right(Result(currDiff, carryFee, currTotalFee, currConstraint, DetailedDiff(parentDiff, txDiffs))), _, _), tx) =>
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
              val minerDiff     = Diff(portfolios = Map(blockGenerator -> minerPortfolio))

              val result = for {
                diff          <- currDiff.combineF(thisTxDiff).flatMap(_.combineF(minerDiff))
                newParentDiff <- parentDiff.combineF(minerDiff)
              } yield Result(
                diff,
                carryFee + carry,
                totalWavesFee,
                updatedConstraint,
                DetailedDiff(newParentDiff, thisTxDiff :: txDiffs)
              )
              TracedResult(result.leftMap(GenericError(_)))
            }
          }
      }
  }

  private def patchesDiff(blockchain: Blockchain): Either[String, Diff] = {
    Seq(CancelAllLeases, CancelLeaseOverflow, CancelInvalidLeaseIn, CancelLeasesToDisabledAliases)
      .foldM(Diff.empty) { case (prevDiff, patch) =>
        patch
          .lift(CompositeBlockchain(blockchain, prevDiff))
          .fold(prevDiff.asRight[String])(prevDiff.combineF)
      }
  }

  private def prepareCaches(blockGenerator: Address, txs: Seq[Transaction], loadCacheData: Seq[Address] => Unit): Unit = {
    val addresses = scala.collection.mutable.Set(blockGenerator)

    txs.foreach {
      case tx: ExchangeTransaction => addresses.addAll(Seq(tx.sender.toAddress, tx.buyOrder.senderAddress, tx.sellOrder.senderAddress))
      case tx: GenesisTransaction  => addresses.add(tx.recipient)
      case tx: InvokeScriptTransaction =>
        addresses.addAll(Seq(tx.senderAddress) ++ (tx.dApp match {
          case addr: Address => Some(addr)
          case _             => None
        }))
      case tx: LeaseTransaction =>
        addresses.addAll(Seq(tx.sender.toAddress) ++ (tx.recipient match {
          case addr: Address => Some(addr)
          case _             => None
        }))
      case tx: MassTransferTransaction =>
        addresses.addAll(Seq(tx.sender.toAddress) ++ tx.transfers.collect { case ParsedTransfer(addr: Address, _) => addr })
      case tx: PaymentTransaction => addresses.addAll(Seq(tx.sender.toAddress, tx.recipient))
      case tx: TransferTransaction =>
        addresses.addAll(Seq(tx.sender.toAddress) ++ (tx.recipient match {
          case addr: Address => Some(addr)
          case _             => None
        }))
      case tx: Authorized => addresses.add(tx.sender.toAddress)
      case _              => ()
    }

    loadCacheData(addresses.toSeq)
  }
}
