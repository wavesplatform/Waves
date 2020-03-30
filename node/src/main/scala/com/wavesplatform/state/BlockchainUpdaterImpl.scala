package com.wavesplatform.state

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.Storage
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.{TxsInBlockchainStats, _}
import com.wavesplatform.mining.{MiningConstraint, MiningConstraints}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.TxValidationError.{BlockAppendError, GenericError, MicroBlockAppendError}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.utils.{ScorexLogging, Time, UnsupportedFeature, forceStopApplication}
import kamon.Kamon
import monix.reactive.subjects.ReplaySubject
import monix.reactive.{Observable, Observer}

class BlockchainUpdaterImpl(
    leveldb: Blockchain with Storage,
    spendableBalanceChanged: Observer[(Address, Asset)],
    wavesSettings: WavesSettings,
    time: Time,
    blockchainUpdateTriggers: BlockchainUpdateTriggers
) extends BlockchainUpdater
    with NG
    with ScorexLogging {

  import com.wavesplatform.state.BlockchainUpdaterImpl._
  import wavesSettings.blockchainSettings.functionalitySettings

  private lazy val maxBlockReadinessAge = wavesSettings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed.toMillis

  private var ngState: Option[NgState]              = Option.empty
  private var restTotalConstraint: MiningConstraint = MiningConstraints(leveldb).total

  private val internalLastBlockInfo = ReplaySubject.createLimited[LastBlockInfo](1)
  private def publishLastBlockInfo(): Unit =
    for (signedBlockHeader <- blockchain.lastBlockHeader) {
      val blockchainReady = signedBlockHeader.header.timestamp + maxBlockReadinessAge > time.correctedTime()
      internalLastBlockInfo.onNext(LastBlockInfo(signedBlockHeader.id(), blockchain.height, blockchain.score, blockchainReady))
    }

  publishLastBlockInfo()

  def liquidBlock(id: ByteStr): Option[Block] = ngState.flatMap(_.forId(id).map(_.totalBlock))

  def liquidBlockMeta: Option[BlockMeta] =
    ngState.map { ng =>
      val b   = ng.totalBlock
      val vrf = if (b.header.version >= Block.ProtoBlockVersion) Some(ng.keyBlock.hitSource) else None
      BlockMeta.fromBlock(b, blockchain.height, ng.fee, ng.keyBlock.reward, vrf)
    }

  def bestLiquidDiff: Option[Diff] = ngState.map(_.totalDiff)

  def blockchain: Blockchain = CompositeBlockchain(leveldb, () => ngState)
  def blockchain(lastBlockId: ByteStr): Blockchain = {
    val snapshot = ngState.flatMap(_.forId(lastBlockId))
    CompositeBlockchain(leveldb, () => snapshot)
  }

  override def isLastBlockId(id: ByteStr): Boolean =
    ngState.fold(leveldb.lastBlockId.contains(id))(_.contains(id))

  override val lastBlockInfo: Observable[LastBlockInfo] = internalLastBlockInfo

  private def featuresApprovedWithBlock(block: Block): Set[Short] = {
    val height = leveldb.height + 1

    val featuresCheckPeriod        = functionalitySettings.activationWindowSize(height)
    val blocksForFeatureActivation = functionalitySettings.blocksForFeatureActivation(height)

    if (height % featuresCheckPeriod == 0) {
      val approvedFeatures = leveldb.featureVotes
        .map { case (feature, votes) => feature -> (if (block.header.featureVotes.contains(feature)) votes + 1 else votes) }
        .filter { case (_, votes) => votes >= blocksForFeatureActivation }
        .keySet
        .filterNot(functionalitySettings.preActivatedFeatures.contains)

      if (approvedFeatures.nonEmpty) log.info(s"${displayFeatures(approvedFeatures)} APPROVED at height $height")

      val unimplementedApproved = approvedFeatures.diff(BlockchainFeatures.implemented)
      if (unimplementedApproved.nonEmpty) {
        log.warn(s"""UNIMPLEMENTED ${displayFeatures(unimplementedApproved)} APPROVED ON BLOCKCHAIN
                    |PLEASE, UPDATE THE NODE AS SOON AS POSSIBLE
                    |OTHERWISE THE NODE WILL BE STOPPED OR FORKED UPON FEATURE ACTIVATION""".stripMargin)
      }

      val activatedFeatures: Set[Short] = leveldb.activatedFeaturesAt(height)

      val unimplementedActivated = activatedFeatures.diff(BlockchainFeatures.implemented)
      if (unimplementedActivated.nonEmpty) {
        log.error(s"UNIMPLEMENTED ${displayFeatures(unimplementedActivated)} ACTIVATED ON BLOCKCHAIN")
        log.error("PLEASE, UPDATE THE NODE IMMEDIATELY")
        if (wavesSettings.featuresSettings.autoShutdownOnUnsupportedFeature) {
          log.error("FOR THIS REASON THE NODE WAS STOPPED AUTOMATICALLY")
          forceStopApplication(UnsupportedFeature)
        } else log.error("OTHERWISE THE NODE WILL END UP ON A FORK")
      }

      approvedFeatures
    } else {

      Set.empty
    }
  }

  private def checkNotImplementedFeatures(): Either[GenericError, Unit] = {
    val height                             = leveldb.height
    val notImplementedFeatures: Set[Short] = leveldb.activatedFeaturesAt(height).diff(BlockchainFeatures.implemented)

    Either
      .cond(
        !wavesSettings.featuresSettings.autoShutdownOnUnsupportedFeature || notImplementedFeatures.isEmpty,
        (),
        GenericError(s"UNIMPLEMENTED ${displayFeatures(notImplementedFeatures)} ACTIVATED ON BLOCKCHAIN, UPDATE THE NODE IMMEDIATELY")
      )
  }

  type DiffR = Option[(BlockDiffer.Result, Seq[Transaction], ByteStr)]

  private def processBlockWithNoNG(block: Block, hitSource: ByteStr, verify: Boolean): Either[ValidationError, DiffR] = leveldb.lastBlockId match {
    case Some(uniqueId) if uniqueId != block.header.reference =>
      val logDetails = s"The referenced block(${block.header.reference})" +
        s" ${if (leveldb.contains(block.header.reference)) "exits, it's not last persisted" else "doesn't exist"}"
      Left(BlockAppendError(s"References incorrect or non-existing block: " + logDetails, block))
    case _ =>
      BlockDiffer
        .fromBlock(
          leveldb,
          leveldb.lastBlock,
          block,
          MiningConstraints(leveldb).total,
          verify
        )
        .map(r => Option((r, Seq.empty[Transaction], hitSource)))
  }

  private def processLiquidBlockVersion(ng: NgState, block: Block, hitSource: ByteStr, verify: Boolean): Either[ValidationError, DiffR] =
    if (block.blockScore() > ng.totalBlock.blockScore()) {
      BlockDiffer
        .fromBlock(leveldb, leveldb.lastBlock, block, MiningConstraints(leveldb).total, verify)
        .map { r =>
          log.trace(s"Better liquid block(score=${block.blockScore()}) received and applied instead of existing(score=${ng.totalBlock.blockScore()})")
          blockchainUpdateTriggers.onMicroBlockRollback(block.header.reference, blockchain.height)
          Some((r, ng.totalBlock.transactionData, hitSource))
        }
    } else if (areVersionsOfSameBlock(block, ng.totalBlock)) {
      if (block.transactionData.lengthCompare(ng.totalBlock.transactionData.size) <= 0) {
        log.trace(s"Existing liquid block is better than new one, discarding $block")
        Right(None)
      } else
        BlockDiffer
          .fromBlock(
            leveldb,
            leveldb.lastBlock,
            block,
            MiningConstraints(leveldb).total,
            verify
          )
          .map { r =>
            log.trace(s"New liquid block is better version of existing, swapping")
            blockchainUpdateTriggers.onMicroBlockRollback(block.header.reference, blockchain.height)
            Some((r, Seq.empty[Transaction], hitSource))
          }
    } else
      Left(
        BlockAppendError(
          s"Competitors liquid block $block(score=${block.blockScore()}) " +
            s"is not better than existing (ng.base ${ng.totalBlock}(score=${ng.totalBlock.blockScore()}))",
          block
        )
      )

  private def processLiquidBlockChild(totalNgState: NgState, block: Block, hitSource: ByteStr, verify: Boolean): Either[ValidationError, DiffR] =
    metrics.forgeBlockTimeStats.measureSuccessful(totalNgState.forge(block.header.reference)) match {
      case None => Left(BlockAppendError(s"References incorrect or non-existing block", block))
      case Some((ngs, discarded)) =>
        if (!verify || ngs.totalBlock.signatureValid()) {
          val blockchainWithProperLiquidBlock = CompositeBlockchain(leveldb, () => Some(ngs))

          if (discarded.nonEmpty) {
            blockchainUpdateTriggers.onMicroBlockRollback(ngs.totalBlock.id(), blockchain.height)
            metrics.microBlockForkStats.increment()
            metrics.microBlockForkHeightStats.record(discarded.size)
          }

          val constraint                    = MiningConstraints(blockchainWithProperLiquidBlock).total
          val prevHitSource                 = ngs.keyBlock.hitSource
          val liquidDiffWithCancelledLeases = ngs.withExpiredLeases

          val newBlockDiffEi = BlockDiffer
            .fromBlock(
              blockchainWithProperLiquidBlock,
              Some(ngs.totalBlock),
              block,
              constraint,
              verify
            )

          newBlockDiffEi.map { newBlockDiff =>
            leveldb.append(liquidDiffWithCancelledLeases, ngs.carry, ngs.fee, ngs.keyBlock.reward, prevHitSource, ngs.totalBlock)
            BlockStats.appended(ngs.totalBlock, ngs.totalDiff.scriptsComplexity)
            TxsInBlockchainStats.record(ngs.totalBlock.transactionData.size)
            Some((newBlockDiff, discarded.flatMap(_.transactionData), hitSource))
          }
        } else {
          val errorText = "Forged block has invalid signature"
          log.error(errorText)
          Left(BlockAppendError(errorText, block))
        }
    }

  override def processBlock(block: Block, hitSource: ByteStr, verify: Boolean = true): Either[ValidationError, Option[DiscardedTransactions]] =
    for {
      _ <- checkNotImplementedFeatures()
      maybeNewLiquidBlock <- ngState match {
        case None => processBlockWithNoNG(block, hitSource, verify)
        case Some(ng) =>
          if (ng.totalBlock.header.reference == block.header.reference) {
            processLiquidBlockVersion(ng, block, hitSource, verify)
          } else {
            processLiquidBlockChild(ng, block, hitSource, verify)
          }
      }
    } yield maybeNewLiquidBlock.map {
      case (differResult, discardedTransactions, hitSource) =>
        val newHeight   = leveldb.height + 1
        val prevNgState = ngState
        val reward      = leveldb.nextBlockReward

        restTotalConstraint = differResult.constraint
        ngState = Some(
          NgState.KeyBlock(
            differResult.diff,
            block,
            differResult.totalFee,
            differResult.carry,
            featuresApprovedWithBlock(block),
            reward,
            hitSource,
            cancelLeases(leveldb, collectLeasesToCancel(leveldb, newHeight))
          )
        )
        notifyChangedSpendable(spendableBalanceChanged, prevNgState, ngState)
        publishLastBlockInfo()

        if ((block.header.timestamp > time.getTimestamp() - maxBlockReadinessAge) || (newHeight % 100 == 0)) {
          log.info(s"New height: $newHeight")
        }

        blockchainUpdateTriggers.onProcessBlock(block, differResult.detailedDiff, reward, leveldb)

        discardedTransactions
    }

  override def removeAfter(blockId: ByteStr): Either[ValidationError, Seq[(Block, ByteStr)]] = {
    log.info(s"Removing blocks after ${blockId.trim} from blockchain")

    val prevNgState = ngState

    val result = prevNgState match {
      case Some(ng) if ng.contains(blockId) =>
        log.trace("Resetting liquid block, no rollback necessary")
        blockchainUpdateTriggers.onMicroBlockRollback(blockId, blockchain.height)
        Right(Seq.empty)
      case Some(ng) if ng.keyBlock.block.header.reference == blockId =>
        log.trace("Discarding liquid block, no rollback necessary")
        blockchainUpdateTriggers.onRollback(blockId, leveldb.height)
        ngState = None
        Right(Seq((ng.totalBlock, ng.keyBlock.hitSource)))
      case maybeNg =>
        leveldb
          .rollbackTo(blockId)
          .map { bs =>
            ngState = None
            blockchainUpdateTriggers.onRollback(blockId, leveldb.height)
            bs ++ maybeNg.map(ng => (ng.totalBlock, ng.keyBlock.hitSource)).toSeq
          }
          .leftMap(err => GenericError(err))
    }

    notifyChangedSpendable(spendableBalanceChanged, prevNgState, ngState)
    publishLastBlockInfo()
    result
  }

  override def processMicroBlock(microBlock: MicroBlock, verify: Boolean = true): Either[ValidationError, BlockId] =
    ngState match {
      case None =>
        Left(MicroBlockAppendError("No base block exists", microBlock))
      case Some(ng) if ng.totalBlock.header.generator.toAddress != microBlock.sender.toAddress =>
        Left(MicroBlockAppendError("Base block has been generated by another account", microBlock))
      case Some(ng) if ng.totalBlock.id() != microBlock.reference =>
        metrics.microMicroForkStats.increment()
        Left(MicroBlockAppendError("It doesn't reference last known microBlock(which exists)", microBlock))
      case Some(ng) =>
        for {
          _ <- microBlock.signaturesValid()
          totalSignatureValid <- ng
            .forId(microBlock.reference)
            .toRight(GenericError(s"No referenced block exists: $microBlock"))
            .map { ngs =>
              ngs.totalBlock
                .appendTransactions(microBlock.transactionData, microBlock.totalResBlockSig)
                .signatureValid()
            }
          _ <- Either
            .cond(
              totalSignatureValid,
              Unit,
              MicroBlockAppendError("Invalid total block signature", microBlock)
            )
          blockDifferResult <- BlockDiffer.fromMicroBlock(
            blockchain,
            leveldb.lastBlockTimestamp,
            microBlock,
            ng.totalBlock.header.timestamp,
            restTotalConstraint,
            verify
          )

        } yield {
          val BlockDiffer.Result(diff, carry, totalFee, updatedMdConstraint, detailedDiff) = blockDifferResult
          restTotalConstraint = updatedMdConstraint
          val newNgState = ng.append(microBlock, diff, totalFee, carry, System.currentTimeMillis())
          val blockId    = newNgState.totalBlock.id()
          blockchainUpdateTriggers.onProcessMicroBlock(microBlock, detailedDiff, blockchain, blockId)
          ngState = Some(newNgState)
          log.info(s"$microBlock appended with id $blockId")
          publishLastBlockInfo()

          for {
            (addr, p) <- diff.portfolios
            assetId   <- p.assetIds
          } spendableBalanceChanged.onNext(addr -> assetId)
          blockId
        }
    }

  override def microBlock(id: BlockId): Option[MicroBlock] =
    for {
      ng <- ngState
      mb <- ng.microBlockForId(id)
    } yield mb

  override def microBlockIds: Seq[BlockId] =
    ngState.fold(Seq.empty[BlockId])(_.microBlockIds)

  override def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo] =
    ngState
      .map(_.bestLastBlockInfo(maxTimestamp))
      .orElse(
        leveldb.lastBlockHeader.map { sh =>
          BlockMinerInfo(sh.header.baseTarget, sh.header.generationSignature, sh.header.timestamp, sh.id())
        }
      )

  def shutdown(): Unit =
    internalLastBlockInfo.onComplete()

  private[this] object metrics {
    val blockMicroForkStats       = Kamon.counter("blockchain-updater.block-micro-fork")
    val microMicroForkStats       = Kamon.counter("blockchain-updater.micro-micro-fork")
    val microBlockForkStats       = Kamon.counter("blockchain-updater.micro-block-fork")
    val microBlockForkHeightStats = Kamon.histogram("blockchain-updater.micro-block-fork-height")
    val forgeBlockTimeStats       = Kamon.timer("blockchain-updater.forge-block-time")
  }
}

object BlockchainUpdaterImpl extends ScorexLogging {
  private def diff(p1: Map[Address, Portfolio], p2: Map[Address, Portfolio]): Map[Address, Portfolio] =
    Monoid.combine(p1, p2.mapValues(_.negate))

  private def displayFeatures(s: Set[Short]): String =
    s"FEATURE${if (s.size > 1) "S" else ""} ${s.mkString(", ")} ${if (s.size > 1) "have been" else "has been"}"

  def areVersionsOfSameBlock(b1: Block, b2: Block): Boolean =
    b1.header.generator == b2.header.generator &&
      b1.header.baseTarget == b2.header.baseTarget &&
      b1.header.reference == b2.header.reference &&
      b1.header.timestamp == b2.header.timestamp

  private def collectLeasesToCancel(leveldb: Blockchain, newHeight: Int): Seq[LeaseTransaction] =
    if (leveldb.isFeatureActivated(BlockchainFeatures.LeaseExpiration, newHeight)) {
      val toHeight = newHeight - leveldb.settings.functionalitySettings.leaseExpiration
      val fromHeight = leveldb.featureActivationHeight(BlockchainFeatures.LeaseExpiration.id) match {
        case Some(activationHeight) if activationHeight == newHeight => 1
        case _                                                       => toHeight
      }
      log.trace(s"Collecting leases created within [$fromHeight, $toHeight]")
      leveldb.collectActiveLeases(_ => true)
    } else Seq.empty

  private def cancelLeases(leveldb: Blockchain, leaseTransactions: Seq[LeaseTransaction]): Map[ByteStr, Diff] =
    (for {
      lt        <- leaseTransactions
      recipient <- leveldb.resolveAlias(lt.recipient).toSeq
    } yield lt.id() -> Diff.empty.copy(
      portfolios = Map(
        lt.sender.toAddress -> Portfolio(0, LeaseBalance(0, -lt.amount), Map.empty),
        recipient           -> Portfolio(0, LeaseBalance(-lt.amount, 0), Map.empty)
      ),
      leaseState = Map(lt.id() -> false)
    )).toMap

  private def notifyChangedSpendable(
      spendableBalanceChanged: Observer[(Address, Asset)],
      prevNgState: Option[NgState],
      newNgState: Option[NgState]
  ): Unit = {
    val changedPortfolios = (prevNgState, newNgState) match {
      case (Some(p), Some(n)) => diff(p.totalDiff.portfolios, n.totalDiff.portfolios)
      case (Some(x), _)       => x.totalDiff.portfolios
      case (_, Some(x))       => x.totalDiff.portfolios
      case _                  => Map.empty
    }

    changedPortfolios.foreach {
      case (addr, p) =>
        p.assetIds.view
          .filter(x => p.spendableBalanceOf(x) != 0)
          .foreach(assetId => spendableBalanceChanged.onNext(addr -> assetId))
    }
  }
}
