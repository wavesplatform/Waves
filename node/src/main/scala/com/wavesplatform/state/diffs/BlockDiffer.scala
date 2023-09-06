package com.wavesplatform.state.diffs

import cats.implicits.{catsSyntaxOption, catsSyntaxSemigroup, toFoldableOps}
import cats.syntax.either.*
import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, BlockSnapshot, MicroBlock, MicroBlockSnapshot}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.state.*
import com.wavesplatform.state.StateSnapshot.monoid
import com.wavesplatform.state.TxStateSnapshotHashBuilder.TxStatusInfo
import com.wavesplatform.state.patch.*
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.{Asset, Authorized, BlockchainUpdater, GenesisTransaction, PaymentTransaction, Transaction}

import scala.collection.immutable.VectorMap

object BlockDiffer {
  final case class Result(
      snapshot: StateSnapshot,
      carry: Long,
      totalFee: Long,
      constraint: MiningConstraint,
      keyBlockSnapshot: StateSnapshot,
      computedStateHash: ByteStr
  )

  case class Fraction(dividend: Int, divider: Int) {
    def apply(l: Long): Long = l / divider * dividend
  }

  val CurrentBlockFeePart: Fraction = Fraction(2, 5)

  def fromBlock(
      blockchain: Blockchain,
      maybePrevBlock: Option[Block],
      block: Block,
      snapshot: Option[BlockSnapshot],
      constraint: MiningConstraint,
      hitSource: ByteStr,
      challengedHitSource: Option[ByteStr] = None,
      loadCacheData: (Set[Address], Set[ByteStr]) => Unit = (_, _) => (),
      verify: Boolean = true,
      enableExecutionLog: Boolean = false,
      txSignParCheck: Boolean = true
  ): Either[ValidationError, Result] = {
    challengedHitSource match {
      case Some(hs) if snapshot.isEmpty =>
        fromBlockTraced(
          blockchain,
          maybePrevBlock,
          block.toOriginal,
          snapshot,
          constraint,
          hs,
          loadCacheData,
          verify,
          enableExecutionLog,
          txSignParCheck
        ).resultE match {
          case Left(_: InvalidStateHash) =>
            fromBlockTraced(
              blockchain,
              maybePrevBlock,
              block,
              snapshot,
              constraint,
              hitSource,
              loadCacheData,
              verify,
              enableExecutionLog,
              txSignParCheck
            ).resultE
          case Left(err) => Left(GenericError(s"Invalid block challenge: $err"))
          case _         => Left(GenericError("Invalid block challenge"))
        }
      case _ =>
        fromBlockTraced(
          blockchain,
          maybePrevBlock,
          block,
          snapshot,
          constraint,
          hitSource,
          loadCacheData,
          verify,
          enableExecutionLog,
          txSignParCheck
        ).resultE
    }
  }

  def fromBlockTraced(
      blockchain: Blockchain,
      maybePrevBlock: Option[Block],
      block: Block,
      snapshot: Option[BlockSnapshot],
      constraint: MiningConstraint,
      hitSource: ByteStr,
      loadCacheData: (Set[Address], Set[ByteStr]) => Unit,
      verify: Boolean,
      enableExecutionLog: Boolean,
      txSignParCheck: Boolean
  ): TracedResult[ValidationError, Result] = {
    val stateHeight        = blockchain.height
    val heightWithNewBlock = stateHeight + 1

    // height switch is next after activation
    val ngHeight          = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)
    val sponsorshipHeight = Sponsorship.sponsoredFeesSwitchHeight(blockchain)

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

    val addressRewardsE: Either[String, (Portfolio, Map[Address, Portfolio], Map[Address, Portfolio])] = for {
      daoAddress        <- blockchain.settings.functionalitySettings.daoAddressParsed
      xtnBuybackAddress <- blockchain.settings.functionalitySettings.xtnBuybackAddressParsed
    } yield {
      val blockRewardShares = BlockRewardCalculator.getBlockRewardShares(
        heightWithNewBlock,
        blockchain.lastBlockReward.getOrElse(0L),
        daoAddress,
        xtnBuybackAddress,
        blockchain
      )
      (
        Portfolio.waves(blockRewardShares.miner),
        daoAddress.fold(Map[Address, Portfolio]())(addr => Map(addr -> Portfolio.waves(blockRewardShares.daoAddress)).filter(_._2.balance > 0)),
        xtnBuybackAddress.fold(Map[Address, Portfolio]())(addr =>
          Map(addr -> Portfolio.waves(blockRewardShares.xtnBuybackAddress)).filter(_._2.balance > 0)
        )
      )
    }

    val blockchainWithNewBlock = SnapshotBlockchain(blockchain, StateSnapshot.empty, block, hitSource, 0, blockchain.lastBlockReward, None)
    val initSnapshotE =
      for {
        feeFromPreviousBlock                             <- feeFromPreviousBlockE
        initialFeeFromThisBlock                          <- initialFeeFromThisBlockE
        totalFee                                         <- initialFeeFromThisBlock.combine(feeFromPreviousBlock)
        (minerReward, daoPortfolio, xtnBuybackPortfolio) <- addressRewardsE
        totalMinerReward                                 <- minerReward.combine(totalFee)
        totalMinerPortfolio = Map(block.sender.toAddress -> totalMinerReward)
        nonMinerRewardPortfolios <- Portfolio.combine(daoPortfolio, xtnBuybackPortfolio)
        totalRewardPortfolios    <- Portfolio.combine(totalMinerPortfolio, nonMinerRewardPortfolios)
        patchesSnapshot = leasePatchesSnapshot(blockchainWithNewBlock)
        resultSnapshot <- patchesSnapshot.addBalances(totalRewardPortfolios, blockchainWithNewBlock)
      } yield resultSnapshot

    for {
      _            <- TracedResult(Either.cond(!verify || block.signatureValid(), (), GenericError(s"Block $block has invalid signature")))
      initSnapshot <- TracedResult(initSnapshotE.leftMap(GenericError(_)))
      prevStateHash = maybePrevBlock.flatMap(_.header.stateHash).getOrElse(blockchain.lastBlockStateHash)
      r <- snapshot match {
        case Some(BlockSnapshot(_, txSnapshots)) =>
          TracedResult.wrapValue(
            apply(blockchainWithNewBlock, prevStateHash, initSnapshot, stateHeight >= ngHeight, block.transactionData, txSnapshots)
          )
        case None =>
          apply(
            blockchainWithNewBlock,
            constraint,
            maybePrevBlock.map(_.header.timestamp),
            prevStateHash,
            initSnapshot,
            stateHeight >= ngHeight,
            block.header.challengedHeader.isDefined,
            block.transactionData,
            loadCacheData,
            verify = verify,
            enableExecutionLog = enableExecutionLog,
            txSignParCheck = txSignParCheck
          )
      }
      _ <- checkStateHash(blockchainWithNewBlock, block.header.stateHash, r.computedStateHash)
    } yield r
  }

  def fromMicroBlock(
      blockchain: Blockchain,
      prevBlockTimestamp: Option[Long],
      prevStateHash: ByteStr,
      micro: MicroBlock,
      snapshot: Option[MicroBlockSnapshot],
      constraint: MiningConstraint,
      loadCacheData: (Set[Address], Set[ByteStr]) => Unit = (_, _) => (),
      verify: Boolean = true,
      enableExecutionLog: Boolean = false
  ): Either[ValidationError, Result] =
    fromMicroBlockTraced(
      blockchain,
      prevBlockTimestamp,
      prevStateHash,
      micro,
      snapshot,
      constraint,
      loadCacheData,
      verify,
      enableExecutionLog
    ).resultE

  private def fromMicroBlockTraced(
      blockchain: Blockchain,
      prevBlockTimestamp: Option[Long],
      prevStateHash: ByteStr,
      micro: MicroBlock,
      snapshot: Option[MicroBlockSnapshot],
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
      r <- snapshot match {
        case Some(MicroBlockSnapshot(_, txSnapshots)) =>
          TracedResult.wrapValue(apply(blockchain, prevStateHash, StateSnapshot.empty, hasNg = true, micro.transactionData, txSnapshots))
        case None =>
          apply(
            blockchain,
            constraint,
            prevBlockTimestamp,
            prevStateHash,
            StateSnapshot.empty,
            hasNg = true,
            hasChallenge = false,
            micro.transactionData,
            loadCacheData,
            verify = verify,
            enableExecutionLog = enableExecutionLog,
            txSignParCheck = true
          )
      }
      _ <- checkStateHash(blockchain, micro.stateHash, r.computedStateHash)
    } yield r
  }

  def maybeApplySponsorship(blockchain: Blockchain, sponsorshipEnabled: Boolean, transactionFee: (Asset, Long)): (Asset, Long) =
    transactionFee match {
      case (ia: IssuedAsset, fee) if sponsorshipEnabled =>
        Waves -> Sponsorship.toWaves(fee, blockchain.assetDescription(ia).get.sponsorship)
      case _ => transactionFee
    }

  def createInitialBlockSnapshot(
      blockchain: BlockchainUpdater & Blockchain,
      miner: Address
  ): Either[ValidationError, StateSnapshot] = {
    val fullReward           = blockchain.computeNextReward.fold(Portfolio.empty)(Portfolio.waves)
    val feeFromPreviousBlock = Portfolio.waves(blockchain.carryFee)

    val daoAddress        = blockchain.settings.functionalitySettings.daoAddressParsed.toOption.flatten
    val xtnBuybackAddress = blockchain.settings.functionalitySettings.xtnBuybackAddressParsed.toOption.flatten

    val rewardShares = BlockRewardCalculator.getBlockRewardShares(
      blockchain.height + 1,
      fullReward.balance,
      daoAddress,
      xtnBuybackAddress,
      blockchain
    )

    Portfolio
      .waves(rewardShares.miner)
      .combine(feeFromPreviousBlock)
      .leftMap(GenericError(_))
      .flatMap { minerReward =>
        val resultPf = Map(miner -> minerReward) ++
          daoAddress.map(_ -> Portfolio.waves(rewardShares.daoAddress)) ++
          xtnBuybackAddress.map(_ -> Portfolio.waves(rewardShares.xtnBuybackAddress))

        StateSnapshot.build(blockchain, portfolios = resultPf.filter(!_._2.isEmpty))
      }
  }

  private[this] def apply(
      blockchain: Blockchain,
      initConstraint: MiningConstraint,
      prevBlockTimestamp: Option[Long],
      prevStateHash: ByteStr,
      initSnapshot: StateSnapshot,
      hasNg: Boolean,
      hasChallenge: Boolean,
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

    val initStateHash =
      if (initSnapshot == StateSnapshot.empty || blockchain.height == 1)
        prevStateHash
      else
        TxStateSnapshotHashBuilder.createHashFromSnapshot(initSnapshot, None).createHash(prevStateHash)

    txs
      .foldLeft(TracedResult(Result(initSnapshot, 0L, 0L, initConstraint, initSnapshot, initStateHash).asRight[ValidationError])) {
        case (acc @ TracedResult(Left(_), _, _), _) => acc
        case (
              TracedResult(
                Right(
                  result @ Result(currSnapshot, carryFee, currTotalFee, currConstraint, keyBlockSnapshot, prevStateHash)
                ),
                _,
                _
              ),
              tx
            ) =>
          val currBlockchain = SnapshotBlockchain(blockchain, currSnapshot)
          val res = txDiffer(currBlockchain, tx).flatMap { txSnapshot =>
            val updatedConstraint = currConstraint.put(currBlockchain, tx, txSnapshot)
            if (updatedConstraint.isOverfilled)
              TracedResult(Left(GenericError(s"Limit of txs was reached: $initConstraint -> $updatedConstraint")))
            else {
              val (feeAsset, feeAmount) = maybeApplySponsorship(currBlockchain, hasSponsorship, tx.assetFee)
              val currentBlockFee       = CurrentBlockFeePart(feeAmount)

              // unless NG is activated, miner has already received all the fee from this block by the time the first
              // transaction is processed (see abode), so there's no need to include tx fee into portfolio.
              // if NG is activated, just give them their 40%
              val minerPortfolio    = if (!hasNg) Portfolio.empty else Portfolio.build(feeAsset, feeAmount).multiply(CurrentBlockFeePart)
              val minerPortfolioMap = Map(blockGenerator -> minerPortfolio)

              // carry is 60% of waves fees the next miner will get. obviously carry fee only makes sense when both
              // NG and sponsorship is active. also if sponsorship is active, feeAsset can only be Waves
              val carry = if (hasNg && hasSponsorship) feeAmount - currentBlockFee else 0

              txSnapshot.addBalances(minerPortfolioMap, currBlockchain).leftMap(GenericError(_)).map { resultTxSnapshot =>
                val (_, txInfo)         = txSnapshot.transactions.head
                val txInfoWithFee       = txInfo.copy(snapshot = resultTxSnapshot.copy(transactions = VectorMap.empty))
                val newKeyBlockSnapshot = keyBlockSnapshot.withTransaction(txInfoWithFee)

                val newSnapshot   = currSnapshot |+| resultTxSnapshot.withTransaction(txInfoWithFee)
                val totalWavesFee = currTotalFee + (if (feeAsset == Waves) feeAmount else 0L)

                Result(
                  newSnapshot,
                  carryFee + carry,
                  totalWavesFee,
                  updatedConstraint,
                  newKeyBlockSnapshot,
                  TxStateSnapshotHashBuilder
                    .createHashFromSnapshot(resultTxSnapshot, Some(TxStatusInfo(txInfo.transaction.id(), txInfo.status)))
                    .createHash(prevStateHash)
                )
              }
            }
          }

          res.copy(resultE = res.resultE.recover {
            case _ if hasChallenge =>
              result.copy(
                snapshot = result.snapshot.bindElidedTransaction(currBlockchain, tx),
                computedStateHash = TxStateSnapshotHashBuilder
                  .createHashFromSnapshot(StateSnapshot.empty, Some(TxStatusInfo(tx.id(), TxMeta.Status.Elided)))
                  .createHash(result.computedStateHash)
              )
          })
      }
  }

  private[this] def apply(
      blockchain: Blockchain,
      prevStateHash: ByteStr,
      initSnapshot: StateSnapshot,
      hasNg: Boolean,
      txs: Seq[Transaction],
      txSnapshots: Seq[(StateSnapshot, TxMeta.Status)]
  ): Result = {
    val hasSponsorship = blockchain.height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain)

    val initStateHash =
      if (initSnapshot == StateSnapshot.empty || blockchain.height == 1)
        prevStateHash
      else
        TxStateSnapshotHashBuilder.createHashFromSnapshot(initSnapshot, None).createHash(prevStateHash)

    txs.zip(txSnapshots).foldLeft(Result(initSnapshot, 0L, 0L, MiningConstraint.Unlimited, initSnapshot, initStateHash)) {
      case (Result(currSnapshot, carryFee, currTotalFee, currConstraint, keyBlockSnapshot, prevStateHash), (tx, (txSnapshot, txStatus))) =>
        val currBlockchain        = SnapshotBlockchain(blockchain, currSnapshot)
        val (feeAsset, feeAmount) = maybeApplySponsorship(currBlockchain, hasSponsorship, tx.assetFee)
        val currentBlockFee       = CurrentBlockFeePart(feeAmount)

        // carry is 60% of waves fees the next miner will get. obviously carry fee only makes sense when both
        // NG and sponsorship is active. also if sponsorship is active, feeAsset can only be Waves
        val carry = if (hasNg && hasSponsorship) feeAmount - currentBlockFee else 0

        val totalWavesFee = currTotalFee + (if (feeAsset == Waves) feeAmount else 0L)

        val nti = NewTransactionInfo.create(tx, txStatus, txSnapshot, currBlockchain)

        Result(
          currSnapshot |+| txSnapshot.withTransaction(nti),
          carryFee + carry,
          totalWavesFee,
          currConstraint,
          keyBlockSnapshot.withTransaction(nti),
          TxStateSnapshotHashBuilder.createHashFromSnapshot(txSnapshot, Some(TxStatusInfo(tx.id(), txStatus))).createHash(prevStateHash)
        )
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
      computedStateHash: ByteStr
  ): TracedResult[ValidationError, Unit] =
    TracedResult(
      Either.cond(
        !blockchain.isFeatureActivated(BlockchainFeatures.TransactionStateSnapshot) || blockStateHash.contains(computedStateHash),
        (),
        InvalidStateHash(blockStateHash)
      )
    )
}
