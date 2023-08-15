package com.wavesplatform.state.diffs

import cats.implicits.toFoldableOps
import cats.syntax.either.*
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
import com.wavesplatform.transaction.{Asset, Authorized, BlockchainUpdater, GenesisTransaction, PaymentTransaction, Transaction}

object BlockDiffer {
  final case class DetailedDiff(parentDiff: Diff, transactionDiffs: List[Diff])
  final case class Result(
      diff: Diff,
      carry: Long,
      totalFee: Long,
      constraint: MiningConstraint,
      detailedDiff: DetailedDiff,
      stateHash: Option[ByteStr]
  )

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
      challengedHitSource: Option[ByteStr] = None,
      loadCacheData: (Set[Address], Set[ByteStr]) => Unit = (_, _) => (),
      verify: Boolean = true,
      enableExecutionLog: Boolean = false,
      txSignParCheck: Boolean = true,
      checkStateHash: Boolean = true // TODO: remove after NODE-2568 merge
  ): Either[ValidationError, Result] = {
    challengedHitSource match {
      case Some(hs) =>
        fromBlockTraced(
          blockchain,
          maybePrevBlock,
          block.toOriginal,
          constraint,
          hs,
          loadCacheData,
          verify,
          enableExecutionLog,
          txSignParCheck,
          checkStateHash
        ).resultE match {
          case Left(_: InvalidStateHash) =>
            fromBlockTraced(
              blockchain,
              maybePrevBlock,
              block,
              constraint,
              hitSource,
              loadCacheData,
              verify,
              enableExecutionLog,
              txSignParCheck,
              checkStateHash
            ).resultE
          case Left(err) => Left(GenericError(s"Invalid block challenge: $err"))
          case _         => Left(GenericError("Invalid block challenge"))
        }
      case _ =>
        fromBlockTraced(
          blockchain,
          maybePrevBlock,
          block,
          constraint,
          hitSource,
          loadCacheData,
          verify,
          enableExecutionLog,
          txSignParCheck,
          checkStateHash
        ).resultE

    }
  }

  def fromBlockTraced(
      blockchain: Blockchain,
      maybePrevBlock: Option[Block],
      block: Block,
      constraint: MiningConstraint,
      hitSource: ByteStr,
      loadCacheData: (Set[Address], Set[ByteStr]) => Unit,
      verify: Boolean,
      enableExecutionLog: Boolean,
      txSignParCheck: Boolean,
      enableStateHash: Boolean // TODO: remove after NODE-2568 merge
  ): TracedResult[ValidationError, Result] = {
    val stateHeight        = blockchain.height
    val heightWithNewBlock = stateHeight + 1

    // height switch is next after activation
    val ngHeight          = blockchain.featureActivationHeight(BlockchainFeatures.NG.id).getOrElse(Int.MaxValue)
    val sponsorshipHeight = Sponsorship.sponsoredFeesSwitchHeight(blockchain)

    val addressRewardsE = for {
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
        daoAddress.fold(Diff.empty)(addr => Diff(portfolios = Map(addr -> Portfolio.waves(blockRewardShares.daoAddress)).filter(_._2.balance > 0))),
        xtnBuybackAddress.fold(Diff.empty)(addr =>
          Diff(portfolios = Map(addr -> Portfolio.waves(blockRewardShares.xtnBuybackAddress)).filter(_._2.balance > 0))
        )
      )
    }

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

    val blockchainWithNewBlock = CompositeBlockchain(blockchain, Diff.empty, block, hitSource, 0, blockchain.lastBlockReward)
    val initDiffE =
      for {
        feeFromPreviousBlock                                 <- feeFromPreviousBlockE
        initialFeeFromThisBlock                              <- initialFeeFromThisBlockE
        (minerReward, daoAddressDiff, xtnBuybackAddressDiff) <- addressRewardsE
        totalReward                                          <- minerReward.combine(initialFeeFromThisBlock).flatMap(_.combine(feeFromPreviousBlock))
        patches                                              <- patchesDiff(blockchainWithNewBlock)
        configAddressesDiff                                  <- daoAddressDiff.combineF(xtnBuybackAddressDiff)
        totalRewardDiff <- Diff(portfolios = Map(block.sender.toAddress -> totalReward)).combineF(configAddressesDiff)
        resultDiff      <- totalRewardDiff.combineF(patches)
      } yield resultDiff.withPortfolios(resultDiff.portfolios.filter(!_._2.isEmpty))

    for {
      _          <- TracedResult(Either.cond(!verify || block.signatureValid(), (), GenericError(s"Block $block has invalid signature")))
      resultDiff <- TracedResult(initDiffE.leftMap(GenericError(_)))
      // TODO: correctly obtain previous state hash on feature activation height
      prevStateHash = if (blockchain.height == 0) Some(TxStateSnapshotHashBuilder.InitStateHash) else maybePrevBlock.flatMap(_.header.stateHash)
      r <- apply(
        blockchainWithNewBlock,
        constraint,
        maybePrevBlock.map(_.header.timestamp),
        prevStateHash,
        resultDiff,
        stateHeight >= ngHeight,
        block.header.challengedHeader.isDefined,
        block.transactionData,
        loadCacheData,
        verify = verify,
        enableExecutionLog = enableExecutionLog,
        txSignParCheck = txSignParCheck
      )
      _ <-
        if (enableStateHash)
          checkStateHash(
            blockchainWithNewBlock,
            block.header.stateHash,
            r.stateHash,
            block.header.challengedHeader.isDefined
          )
        else TracedResult(Right(()))
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
    fromMicroBlockTraced(
      blockchain,
      prevBlockTimestamp,
      prevStateHash,
      micro,
      constraint,
      loadCacheData,
      verify,
      enableExecutionLog
    ).resultE

  def fromMicroBlockTraced(
      blockchain: Blockchain,
      prevBlockTimestamp: Option[Long],
      prevStateHash: Option[ByteStr],
      micro: MicroBlock,
      constraint: MiningConstraint,
      loadCacheData: (Set[Address], Set[ByteStr]) => Unit = (_, _) => (),
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
        prevStateHash,
        Diff.empty,
        hasNg = true,
        hasChallenge = false,
        micro.transactionData,
        loadCacheData,
        verify = verify,
        enableExecutionLog = enableExecutionLog,
        txSignParCheck = txSignParCheck
      )
      _ <- checkStateHash(blockchain, micro.stateHash, r.stateHash, hasChallenge = false)
    } yield r
  }

  def maybeApplySponsorship(blockchain: Blockchain, sponsorshipEnabled: Boolean, transactionFee: (Asset, Long)): (Asset, Long) =
    transactionFee match {
      case (ia: IssuedAsset, fee) if sponsorshipEnabled =>
        Waves -> Sponsorship.toWaves(fee, blockchain.assetDescription(ia).get.sponsorship)
      case _ => transactionFee
    }

  def createInitialBlockDiff(
      blockchain: BlockchainUpdater & Blockchain,
      miner: Address
  ): Either[String, Diff] = {
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
      .map { minerReward =>
        val resultPf = Map(miner -> minerReward) ++
          daoAddress.map(_ -> Portfolio.waves(rewardShares.daoAddress)) ++
          xtnBuybackAddress.map(_ -> Portfolio.waves(rewardShares.xtnBuybackAddress))

        Diff(portfolios = resultPf.filter(!_._2.isEmpty))
      }
  }

  private[this] def apply(
      blockchain: Blockchain,
      initConstraint: MiningConstraint,
      prevBlockTimestamp: Option[Long],
      prevStateHash: Option[ByteStr],
      initDiff: Diff,
      hasNg: Boolean,
      hasChallenge: Boolean,
      txs: Seq[Transaction],
      loadCacheData: (Set[Address], Set[ByteStr]) => Unit,
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

    if (verify && txSignParCheck)
      ParSignatureChecker.checkTxSignatures(txs, rideV6Activated)

    prepareCaches(blockGenerator, txs, loadCacheData)

    val initStateHash =
      if (blockchain.isFeatureActivated(BlockchainFeatures.TransactionStateSnapshot)) {
        if (initDiff == Diff.empty || blockchain.height == 1)
          prevStateHash
        else
          prevStateHash.map(TxStateSnapshotHashBuilder.createHashFromDiff(blockchain, initDiff).createHash(_))
      } else None

    txs
      .foldLeft(
        TracedResult(Result(initDiff, 0L, 0L, initConstraint, DetailedDiff(initDiff, Nil), initStateHash).asRight[ValidationError])
      ) {
        case (acc @ TracedResult(Left(_), _, _), _) => acc
        case (
              TracedResult(
                Right(
                  result @ Result(currDiff, carryFee, currTotalFee, currConstraint, DetailedDiff(parentDiff, txDiffs), prevStateHash)
                ),
                _,
                _
              ),
              tx
            ) =>
          val currBlockchain = CompositeBlockchain(blockchain, currDiff)
          val res = txDiffer(currBlockchain, tx).flatMap { thisTxDiff =>
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
                fullTxDiff    <- thisTxDiff.combineF(minerDiff)
              } yield Result(
                diff,
                carryFee + carry,
                totalWavesFee,
                updatedConstraint,
                DetailedDiff(newParentDiff, thisTxDiff :: txDiffs),
                prevStateHash.map(TxStateSnapshotHashBuilder.createHashFromDiff(currBlockchain, fullTxDiff).createHash(_))
              )
              TracedResult(result.leftMap(GenericError(_)))
            }
          }

          res.copy(resultE = res.resultE.recover {
            case _ if hasChallenge =>
              result.copy(diff = result.diff.bindElidedTransaction(blockchain, tx))
          })
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
      computedStateHash: Option[ByteStr],
      hasChallenge: Boolean
  ): TracedResult[ValidationError, Unit] =
    TracedResult(
      Either.cond(
        !blockchain.isFeatureActivated(BlockchainFeatures.TransactionStateSnapshot) || computedStateHash.exists(blockStateHash.contains),
        (),
        if (hasChallenge) GenericError("Invalid block challenge")
        else
          InvalidStateHash(blockStateHash)
      )
    )
}
