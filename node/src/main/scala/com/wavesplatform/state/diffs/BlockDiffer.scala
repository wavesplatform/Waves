package com.wavesplatform.state.diffs

import cats.implicits.{catsSyntaxOption, catsSyntaxSemigroup, toBifunctorOps, toFoldableOps}
import cats.syntax.either.catsSyntaxEitherId
import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.RocksDBWriter
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.state.*
import com.wavesplatform.state.StateSnapshot.monoid
import com.wavesplatform.state.patch.*
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.{Asset, Authorized, GenesisTransaction, PaymentTransaction, Transaction}

object BlockDiffer {
  final case class Result(
      snapshot: StateSnapshot,
      carry: Long,
      totalFee: Long,
      constraint: MiningConstraint,
      minerSnapshot: StateSnapshot,
      stateHash: Option[ByteStr]
  )

  case class Fraction(dividend: Int, divider: Int) {
    def apply(l: Long): Long = l / divider * dividend
  }

  case object InvalidStateHash extends ValidationError

  val CurrentBlockFeePart: Fraction = Fraction(2, 5)

  def fromBlock(
      blockchain: Blockchain,
      maybePrevBlock: Option[Block],
      block: Block,
      constraint: MiningConstraint,
      hitSource: ByteStr,
      loadCacheData: (Set[Address], Set[ByteStr]) => Unit = (_, _) => (),
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
      loadCacheData: (Set[Address], Set[ByteStr]) => Unit,
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

    val blockchainWithNewBlock = SnapshotBlockchain(blockchain, StateSnapshot.empty, block, hitSource, 0, None)
    val initSnapshotE =
      for {
        feeFromPreviousBlock    <- feeFromPreviousBlockE
        initialFeeFromThisBlock <- initialFeeFromThisBlockE
        totalReward             <- minerReward.combine(initialFeeFromThisBlock).flatMap(_.combine(feeFromPreviousBlock))
        patchesSnapshot = leasePatchesSnapshot(blockchainWithNewBlock)
      } yield patchesSnapshot.addBalances(Map(block.sender.toAddress -> totalReward), blockchainWithNewBlock)

    for {
      _            <- TracedResult(Either.cond(!verify || block.signatureValid(), (), GenericError(s"Block $block has invalid signature")))
      initSnapshot <- TracedResult(initSnapshotE.leftMap(GenericError(_)))
      r <- apply(
        blockchainWithNewBlock,
        constraint,
        maybePrevBlock.map(_.header.timestamp),
        // TODO: correctly obtain previous state hash on feature activation height
        if (blockchain.height == 0) Some(TxStateSnapshotHashBuilder.InitStateHash) else maybePrevBlock.flatMap(_.header.stateHash),
        initSnapshot,
        stateHeight >= ngHeight,
        block.transactionData,
        loadCacheData,
        verify = verify,
        enableExecutionLog = enableExecutionLog,
        txSignParCheck = txSignParCheck
      )
      _ <- checkStateHash(blockchainWithNewBlock, block.header.stateHash, r.stateHash)
    } yield r
  }

  def fromMicroBlock(
      blockchain: Blockchain,
      prevBlockTimestamp: Option[Long],
      prevStateHash: Option[ByteStr],
      micro: MicroBlock,
      constraint: MiningConstraint,
      loadCacheData: (Set[Address], Set[ByteStr]) => Unit = (_, _) => (),
      verify: Boolean = true,
      enableExecutionLog: Boolean = false
  ): Either[ValidationError, Result] =
    fromMicroBlockTraced(blockchain, prevBlockTimestamp, prevStateHash, micro, constraint, loadCacheData, verify, enableExecutionLog).resultE

  private def fromMicroBlockTraced(
      blockchain: Blockchain,
      prevBlockTimestamp: Option[Long],
      prevStateHash: Option[ByteStr],
      micro: MicroBlock,
      constraint: MiningConstraint,
      loadCacheData: (Set[Address], Set[ByteStr]) => Unit,
      verify: Boolean,
      enableExecutionLog: Boolean
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
        prevStateHash,
        StateSnapshot.empty,
        hasNg = true,
        micro.transactionData,
        loadCacheData,
        verify = verify,
        enableExecutionLog = enableExecutionLog,
        txSignParCheck = true
      )
      _ <- checkStateHash(blockchain, micro.stateHash, r.stateHash)
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
      prevStateHash: Option[ByteStr],
      initSnapshot: StateSnapshot,
      hasNg: Boolean,
      txs: Seq[Transaction],
      loadCacheData: (Set[Address], Set[ByteStr]) => Unit,
      verify: Boolean,
      enableExecutionLog: Boolean,
      txSignParCheck: Boolean
  ): TracedResult[ValidationError, Result] = {
    val currentBlockHeight = blockchain.height
    val timestamp          = blockchain.lastBlockTimestamp.get
    val blockGenerator     = blockchain.lastBlockHeader.get.header.generator.toAddress
    val rideV6Activated    = blockchain.isFeatureActivated(BlockchainFeatures.RideV6)

    val txDiffer       = TransactionDiffer(prevBlockTimestamp, timestamp, verify, enableExecutionLog = enableExecutionLog) _
    val hasSponsorship = currentBlockHeight >= Sponsorship.sponsoredFeesSwitchHeight(blockchain)

    if (verify && txSignParCheck)
      ParSignatureChecker.checkTxSignatures(txs, rideV6Activated)

    prepareCaches(blockGenerator, txs, loadCacheData)

    txs
      .foldLeft(TracedResult(Result(initSnapshot, 0L, 0L, initConstraint, initSnapshot, prevStateHash).asRight[ValidationError])) {
        case (acc @ TracedResult(Left(_), _, _), _) => acc
        case (
              TracedResult(
                Right(Result(currSnapshot, carryFee, currTotalFee, currConstraint, minerSnapshot, prevStateHashOpt)),
                _,
                _
              ),
              tx
            ) =>
          val currBlockchain = SnapshotBlockchain(blockchain, currSnapshot)
          txDiffer(currBlockchain, tx).flatMap { thisTxDiff =>
            val updatedConstraint = currConstraint.put(currBlockchain, tx, thisTxDiff)
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

              val txSnapshot  = StateSnapshot.fromDiff(thisTxDiff, currBlockchain).withTransaction(thisTxDiff.transactions.head)
              val newSnapshot = currSnapshot |+| txSnapshot.addBalances(Map(blockGenerator -> minerPortfolio), currBlockchain)
              val newMinerSnapshot =
                minerSnapshot
                  .withTransaction(thisTxDiff.transactions.head)
                  .addBalances(Map(blockGenerator -> minerPortfolio), currBlockchain)

              val totalWavesFee = currTotalFee + (if (feeAsset == Waves) feeAmount else 0L)

              val result = Result(
                newSnapshot,
                carryFee + carry,
                totalWavesFee,
                updatedConstraint,
                newMinerSnapshot,
                prevStateHashOpt
                  .map(prevStateHash => TxStateSnapshotHashBuilder.createHashFromTxDiff(currBlockchain, thisTxDiff).createHash(prevStateHash))
              )
              TracedResult(result.asRight)
            }
          }
      }
  }

  private def leasePatchesSnapshot(blockchain: Blockchain): StateSnapshot =
    Seq(CancelAllLeases, CancelLeaseOverflow, CancelInvalidLeaseIn, CancelLeasesToDisabledAliases)
      .foldLeft(StateSnapshot.empty) { case (prevSnapshot, patch) =>
        prevSnapshot |+| patch.lift(SnapshotBlockchain(blockchain, prevSnapshot)).orEmpty
      }

  private def prepareCaches(blockGenerator: Address, txs: Seq[Transaction], loadCacheData: (Set[Address], Set[ByteStr]) => Unit): Unit = {
    val addresses = Set.newBuilder[Address].addOne(blockGenerator)
    val orders    = Set.newBuilder[ByteStr]

    txs.foreach {
      case tx: ExchangeTransaction =>
        addresses.addAll(Seq(tx.sender.toAddress, tx.buyOrder.senderAddress, tx.sellOrder.senderAddress))
        orders.addOne(tx.buyOrder.id()).addOne(tx.sellOrder.id())
      case tx: GenesisTransaction => addresses.addOne(tx.recipient)
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
      case tx: Authorized => addresses.addOne(tx.sender.toAddress)
      case _              => ()
    }

    loadCacheData(addresses.result(), orders.result())
  }

  private def checkStateHash(
      blockchain: Blockchain,
      blockStateHash: Option[ByteStr],
      computedStateHash: Option[ByteStr]
  ): TracedResult[ValidationError, Unit] =
    TracedResult(
      Either.cond(
        !blockchain.isFeatureActivated(BlockchainFeatures.TransactionStateSnapshot) || computedStateHash.exists(blockStateHash.contains),
        (),
        InvalidStateHash
      )
    )
}
