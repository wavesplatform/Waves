package com.wavesplatform.state

import java.util.concurrent.locks.{Lock, ReentrantReadWriteLock}

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.block.Block.{BlockId, BlockInfo}
import com.wavesplatform.block.{Block, BlockHeader, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.metrics.{TxsInBlockchainStats, _}
import com.wavesplatform.mining.{MiningConstraint, MiningConstraints}
import com.wavesplatform.settings.{BlockchainSettings, WavesSettings}
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.extensions.composite.{CompositeAddressTransactions, CompositeDistributions}
import com.wavesplatform.state.extensions.{AddressTransactions, Distributions}
import com.wavesplatform.state.reader.{CompositeBlockchain, LeaseDetails}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{BlockAppendError, GenericError, MicroBlockAppendError}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.utils.{ScorexLogging, Time, UnsupportedFeature, forceStopApplication}
import kamon.Kamon
import monix.reactive.subjects.ReplaySubject
import monix.reactive.{Observable, Observer}

class BlockchainUpdaterImpl(
    private val blockchain: LevelDBWriter,
    spendableBalanceChanged: Observer[(Address, Asset)],
    wavesSettings: WavesSettings,
    time: Time,
    blockchainUpdated: Observer[BlockchainUpdated]
) extends BlockchainUpdater
    with NG
    with ScorexLogging {

  import com.wavesplatform.state.BlockchainUpdaterImpl._
  import wavesSettings.blockchainSettings.functionalitySettings

  private def inLock[R](l: Lock, f: => R): R = {
    l.lockInterruptibly()
    try f
    finally l.unlock()
  }

  private val lock                     = new ReentrantReadWriteLock
  private def writeLock[B](f: => B): B = inLock(lock.writeLock(), f)
  private def readLock[B](f: => B): B  = inLock(lock.readLock(), f)

  private lazy val maxBlockReadinessAge = wavesSettings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed.toMillis

  private var ngState: Option[NgState]              = Option.empty
  private var restTotalConstraint: MiningConstraint = MiningConstraints(blockchain, blockchain.height).total

  private val internalLastBlockInfo = ReplaySubject.createLimited[LastBlockInfo](1)

  private def publishLastBlockInfo(): Unit =
    for (id <- lastBlockId; ts <- ngState.map(_.base.header.timestamp).orElse(blockchain.lastBlockTimestamp)) {
      val blockchainReady = ts + maxBlockReadinessAge > time.correctedTime()
      internalLastBlockInfo.onNext(LastBlockInfo(id, height, score, blockchainReady))
    }

  publishLastBlockInfo()

  @noinline
  def bestLiquidDiff: Option[Diff] = readLock(ngState.map(_.bestLiquidDiff))

  override val settings: BlockchainSettings = wavesSettings.blockchainSettings

  override def isLastBlockId(id: ByteStr): Boolean = readLock {
    ngState.exists(_.contains(id)) || lastBlock.exists(_.uniqueId == id)
  }

  override val lastBlockInfo: Observable[LastBlockInfo] = internalLastBlockInfo

  private def displayFeatures(s: Set[Short]): String =
    s"FEATURE${if (s.size > 1) "S" else ""} ${s.mkString(", ")} ${if (s.size > 1) "have been" else "has been"}"

  private def featuresApprovedWithBlock(block: Block): Set[Short] = {
    val height = blockchain.height + 1

    val featuresCheckPeriod        = functionalitySettings.activationWindowSize(height)
    val blocksForFeatureActivation = functionalitySettings.blocksForFeatureActivation(height)

    if (height % featuresCheckPeriod == 0) {
      val approvedFeatures = blockchain
        .featureVotes(height)
        .map { case (feature, votes) => feature -> (if (block.header.featureVotes.contains(feature)) votes + 1 else votes) }
        .filter { case (_, votes) => votes >= blocksForFeatureActivation }
        .keySet

      if (approvedFeatures.nonEmpty) log.info(s"${displayFeatures(approvedFeatures)} APPROVED at height $height")

      val unimplementedApproved = approvedFeatures.diff(BlockchainFeatures.implemented)
      if (unimplementedApproved.nonEmpty) {
        log.warn(s"UNIMPLEMENTED ${displayFeatures(unimplementedApproved)} APPROVED ON BLOCKCHAIN")
        log.warn("PLEASE, UPDATE THE NODE AS SOON AS POSSIBLE")
        log.warn("OTHERWISE THE NODE WILL BE STOPPED OR FORKED UPON FEATURE ACTIVATION")
      }

      val activatedFeatures: Set[Short] = blockchain.activatedFeaturesAt(height)

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

  private def nextReward(): Option[Long] = {
    val settings   = this.settings.rewardsSettings
    val nextHeight = this.height + 1

    blockchain
      .featureActivationHeight(BlockchainFeatures.BlockReward.id)
      .filter(_ <= nextHeight)
      .flatMap { activatedAt =>
        val mayBeReward     = lastBlockReward
        val mayBeTimeToVote = nextHeight - activatedAt

        mayBeReward match {
          case Some(reward) if mayBeTimeToVote > 0 && mayBeTimeToVote % settings.term == 0 =>
            Some((blockRewardVotes(this.height).filter(_ >= 0), reward))
          case None if mayBeTimeToVote >= 0 =>
            Some((Seq(), settings.initial))
          case _ => None
        }
      }
      .flatMap {
        case (votes, currentReward) =>
          val lt        = votes.count(_ < currentReward)
          val gt        = votes.count(_ > currentReward)
          val threshold = settings.votingInterval / 2 + 1

          if (lt >= threshold)
            Some(math.max(currentReward - settings.minIncrement, 0))
          else if (gt >= threshold)
            Some(currentReward + settings.minIncrement)
          else
            Some(currentReward)
      }
      .orElse(lastBlockReward)
  }

  override def processBlock(block: Block, hitSource: ByteStr, verify: Boolean = true): Either[ValidationError, Option[DiscardedTransactions]] =
    writeLock {
      val height                             = blockchain.height
      val notImplementedFeatures: Set[Short] = blockchain.activatedFeaturesAt(height).diff(BlockchainFeatures.implemented)

      Either
        .cond(
          !wavesSettings.featuresSettings.autoShutdownOnUnsupportedFeature || notImplementedFeatures.isEmpty,
          (),
          GenericError(s"UNIMPLEMENTED ${displayFeatures(notImplementedFeatures)} ACTIVATED ON BLOCKCHAIN, UPDATE THE NODE IMMEDIATELY")
        )
        .flatMap[ValidationError, Option[DiscardedTransactions]](
          _ =>
            (ngState match {
              case None =>
                blockchain.lastBlockId match {
                  case Some(uniqueId) if uniqueId != block.header.reference =>
                    val logDetails = s"The referenced block(${block.header.reference})" +
                      s" ${if (blockchain.contains(block.header.reference)) "exits, it's not last persisted" else "doesn't exist"}"
                    Left(BlockAppendError(s"References incorrect or non-existing block: " + logDetails, block))
                  case lastBlockId =>
                    val height            = lastBlockId.fold(0)(blockchain.unsafeHeightOf)
                    val miningConstraints = MiningConstraints(blockchain, height)
                    val reward            = nextReward()
                    BlockDiffer
                      .fromBlock(
                        CompositeBlockchain(blockchain, carry = blockchain.carryFee, reward = reward),
                        blockchain.lastBlock,
                        block,
                        miningConstraints.total,
                        verify
                      )
                      .map(r => Option((r, Seq.empty[Transaction], reward, hitSource)))
                }
              case Some(ng) =>
                if (ng.base.header.reference == block.header.reference) {
                  if (block.blockScore() > ng.base.blockScore()) {
                    val height            = blockchain.unsafeHeightOf(ng.base.header.reference)
                    val miningConstraints = MiningConstraints(blockchain, height)

                    BlockchainUpdateNotifier.notifyMicroBlockRollback(blockchainUpdated, block.header.reference, height)

                    BlockDiffer
                      .fromBlock(
                        CompositeBlockchain(blockchain, carry = blockchain.carryFee, reward = ng.reward),
                        blockchain.lastBlock,
                        block,
                        miningConstraints.total,
                        verify
                      )
                      .map { r =>
                        log.trace(
                          s"Better liquid block(score=${block.blockScore()}) received and applied instead of existing(score=${ng.base.blockScore()})"
                        )
                        Some((r, ng.transactions, ng.reward, hitSource))
                      }
                  } else if (areVersionsOfSameBlock(block, ng.base)) {
                    if (block.transactionData.lengthCompare(ng.transactions.size) <= 0) {
                      log.trace(s"Existing liquid block is better than new one, discarding $block")
                      Right(None)
                    } else {
                      log.trace(s"New liquid block is better version of existing, swapping")
                      val height            = blockchain.unsafeHeightOf(ng.base.header.reference)
                      val miningConstraints = MiningConstraints(blockchain, height)

                      BlockchainUpdateNotifier
                        .notifyMicroBlockRollback(blockchainUpdated, block.header.reference, height)

                      BlockDiffer
                        .fromBlock(
                          CompositeBlockchain(blockchain, carry = blockchain.carryFee, reward = ng.reward),
                          blockchain.lastBlock,
                          block,
                          miningConstraints.total,
                          verify
                        )
                        .map(r => Some((r, Seq.empty[Transaction], ng.reward, hitSource)))
                    }
                  } else
                    Left(
                      BlockAppendError(
                        s"Competitors liquid block $block(score=${block.blockScore()}) is not better than existing (ng.base ${ng.base}(score=${ng.base
                          .blockScore()}))",
                        block
                      )
                    )
                } else
                  metrics.forgeBlockTimeStats.measureSuccessful(ng.totalDiffOf(block.header.reference)) match {
                    case None => Left(BlockAppendError(s"References incorrect or non-existing block", block))
                    case Some((referencedForgedBlock, referencedLiquidDiff, carry, totalFee, discarded)) =>
                      if (!verify || referencedForgedBlock.signaturesValid().isRight) {
                        val height = blockchain.heightOf(referencedForgedBlock.header.reference).getOrElse(0)

                        if (discarded.nonEmpty) {
                          BlockchainUpdateNotifier
                            .notifyMicroBlockRollback(blockchainUpdated, referencedForgedBlock.uniqueId, height)
                          metrics.microBlockForkStats.increment()
                          metrics.microBlockForkHeightStats.record(discarded.size)
                        }

                        val constraint: MiningConstraint = {
                          val miningConstraints = MiningConstraints(blockchain, height)
                          miningConstraints.total
                        }

                        val prevReward = ng.reward
                        val reward     = nextReward()

                        val prevHitSource = ng.hitSource

                        val liquidDiffWithCancelledLeases = ng.cancelExpiredLeases(referencedLiquidDiff)

                        val diff = BlockDiffer
                          .fromBlock(
                            CompositeBlockchain(blockchain, Some(liquidDiffWithCancelledLeases), Some(referencedForgedBlock), carry, reward),
                            Some(referencedForgedBlock),
                            block,
                            constraint,
                            verify
                          )

                        diff.map { hardenedDiff =>
                          blockchain.append(liquidDiffWithCancelledLeases, carry, totalFee, prevReward, prevHitSource, referencedForgedBlock)
                          BlockStats.appended(referencedForgedBlock, referencedLiquidDiff.scriptsComplexity)
                          TxsInBlockchainStats.record(ng.transactions.size)
                          Some((hardenedDiff, discarded.flatMap(_.transactionData), reward, hitSource))
                        }
                      } else {
                        val errorText = s"Forged block has invalid signature: base: ${ng.base}, requested reference: ${block.header.reference}"
                        log.error(errorText)
                        Left(BlockAppendError(errorText, block))
                      }
                  }
            }).map {
              _ map {
                case (BlockDiffer.Result(newBlockDiff, carry, totalFee, updatedTotalConstraint, detailedDiff), discarded, reward, hitSource) =>
                  val newHeight   = blockchain.height + 1
                  val prevNgState = ngState

                  restTotalConstraint = updatedTotalConstraint
                  ngState = Some(
                    new NgState(
                      block,
                      newBlockDiff,
                      carry,
                      totalFee,
                      featuresApprovedWithBlock(block),
                      reward,
                      hitSource,
                      cancelLeases(collectLeasesToCancel(newHeight))
                    )
                  )
                  notifyChangedSpendable(prevNgState, ngState)
                  publishLastBlockInfo()

                  if ((block.header.timestamp > time
                        .getTimestamp() - wavesSettings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed.toMillis) || (newHeight % 100 == 0)) {
                    log.info(s"New height: $newHeight")
                  }

                  BlockchainUpdateNotifier.notifyProcessBlock(blockchainUpdated, block, detailedDiff, blockchain)

                  discarded
              }
            }
        )
    }

  private def collectLeasesToCancel(newHeight: Int): Seq[LeaseTransaction] =
    if (blockchain.isFeatureActivated(BlockchainFeatures.LeaseExpiration, newHeight)) {
      val toHeight = newHeight - blockchain.settings.functionalitySettings.leaseExpiration
      val fromHeight = blockchain.featureActivationHeight(BlockchainFeatures.LeaseExpiration.id) match {
        case Some(activationHeight) if activationHeight == newHeight => 1
        case _                                                       => toHeight
      }
      log.trace(s"Collecting leases created within [$fromHeight, $toHeight]")
      blockchain.collectActiveLeases(fromHeight, toHeight)(_ => true)
    } else Seq.empty

  private def cancelLeases(leaseTransactions: Seq[LeaseTransaction]): Map[ByteStr, Diff] =
    (for {
      lt        <- leaseTransactions
      recipient <- blockchain.resolveAlias(lt.recipient).toSeq
    } yield lt.id() -> Diff.empty.copy(
      portfolios = Map(
        lt.sender.toAddress -> Portfolio(0, LeaseBalance(0, -lt.amount), Map.empty),
        recipient           -> Portfolio(0, LeaseBalance(-lt.amount, 0), Map.empty)
      ),
      leaseState = Map(lt.id() -> false)
    )).toMap

  override def removeAfter(blockId: ByteStr): Either[ValidationError, Seq[(Block, ByteStr)]] = writeLock {
    log.info(s"Removing blocks after ${blockId.trim} from blockchain")

    val prevNgState = ngState
    val result = if (prevNgState.exists(_.contains(blockId))) {
      log.trace("Resetting liquid block, no rollback is necessary")
      BlockchainUpdateNotifier.notifyMicroBlockRollback(blockchainUpdated, blockId, blockchain.height)
      Right(Seq.empty)
    } else {
      val discardedNgBlock = prevNgState.map(ng => (ng.bestLiquidBlock, ng.hitSource)).toSeq
      ngState = None
      blockchain
        .rollbackTo(blockId)
        .map { bs =>
          BlockchainUpdateNotifier.notifyRollback(blockchainUpdated, blockId, blockchain.height)
          bs ++ discardedNgBlock
        }
        .leftMap(err => GenericError(err))
    }

    notifyChangedSpendable(prevNgState, ngState)
    publishLastBlockInfo()
    result
  }

  private def notifyChangedSpendable(prevNgState: Option[NgState], newNgState: Option[NgState]): Unit = {
    val changedPortfolios = (prevNgState, newNgState) match {
      case (Some(p), Some(n)) => diff(p.bestLiquidDiff.portfolios, n.bestLiquidDiff.portfolios)
      case (Some(x), _)       => x.bestLiquidDiff.portfolios
      case (_, Some(x))       => x.bestLiquidDiff.portfolios
      case _                  => Map.empty
    }

    changedPortfolios.foreach {
      case (addr, p) =>
        p.assetIds.view
          .filter(x => p.spendableBalanceOf(x) != 0)
          .foreach(assetId => spendableBalanceChanged.onNext(addr -> assetId))
    }
  }

  private def diff(p1: Map[Address, Portfolio], p2: Map[Address, Portfolio]) = Monoid.combine(p1, p2.map { case (k, v) => k -> v.negate })

  override def processMicroBlock(microBlock: MicroBlock, verify: Boolean = true): Either[ValidationError, Unit] = writeLock {
    ngState match {
      case None =>
        Left(MicroBlockAppendError("No base block exists", microBlock))
      case Some(ng) if ng.base.header.generator.toAddress != microBlock.sender.toAddress =>
        Left(MicroBlockAppendError("Base block has been generated by another account", microBlock))
      case Some(ng) =>
        ng.lastMicroBlock match {
          case None if ng.base.uniqueId != microBlock.prevResBlockSig =>
            metrics.blockMicroForkStats.increment()
            Left(MicroBlockAppendError("It's first micro and it doesn't reference base block(which exists)", microBlock))
          case Some(prevMicro) if prevMicro.totalResBlockSig != microBlock.prevResBlockSig =>
            metrics.microMicroForkStats.increment()
            Left(MicroBlockAppendError("It doesn't reference last known microBlock(which exists)", microBlock))
          case _ =>
            for {
              _ <- microBlock.signaturesValid()
              totalSignatureValid <- ng
                .totalDiffOf(microBlock.prevResBlockSig)
                .toRight(GenericError(s"No referenced block exists: $microBlock"))
                .map {
                  case (accumulatedBlock, _, _, _, _) =>
                    Block
                      .create(accumulatedBlock, accumulatedBlock.transactionData ++ microBlock.transactionData, microBlock.totalResBlockSig)
                      .signatureValid()
                }
              _ <- Either
                .cond(
                  totalSignatureValid,
                  Unit,
                  MicroBlockAppendError("Invalid total block signature", microBlock)
                )
              blockDifferResult <- {
                BlockDiffer.fromMicroBlock(this, blockchain.lastBlockTimestamp, microBlock, ng.base.header.timestamp, restTotalConstraint, verify)
              }
            } yield {
              val BlockDiffer.Result(diff, carry, totalFee, updatedMdConstraint, detailedDiff) = blockDifferResult
              BlockchainUpdateNotifier.notifyProcessMicroBlock(blockchainUpdated, microBlock, detailedDiff, blockchain)
              restTotalConstraint = updatedMdConstraint
              ng.append(microBlock, diff, carry, totalFee, System.currentTimeMillis)
              log.info(s"$microBlock appended")
              internalLastBlockInfo.onNext(LastBlockInfo(microBlock.totalResBlockSig, height, score, ready = true))

              for {
                (addr, p) <- diff.portfolios
                assetId   <- p.assetIds
              } spendableBalanceChanged.onNext(addr -> assetId)
            }
        }
    }
  }

  def shutdown(): Unit = {
    internalLastBlockInfo.onComplete()
  }

  private def newlyApprovedFeatures = ngState.fold(Map.empty[Short, Int])(_.approvedFeatures.map(_ -> height).toMap)

  override def approvedFeatures: Map[Short, Int] = readLock {
    newlyApprovedFeatures ++ blockchain.approvedFeatures
  }

  override def activatedFeatures: Map[Short, Int] = readLock {
    newlyApprovedFeatures.mapValues(_ + functionalitySettings.activationWindowSize(height)) ++ blockchain.activatedFeatures
  }

  override def featureVotes(height: Int): Map[Short, Int] = readLock {
    val innerVotes = blockchain.featureVotes(height)
    ngState match {
      case Some(ng) if this.height <= height =>
        val ngVotes = ng.base.header.featureVotes.map { featureId =>
          featureId -> (innerVotes.getOrElse(featureId, 0) + 1)
        }.toMap

        innerVotes ++ ngVotes
      case _ => innerVotes
    }
  }

  override def blockReward(height: Int): Option[Long] = readLock {
    blockchain.blockReward(height) match {
      case r @ Some(_) => r
      case None        => ngState.collect { case ng if blockchain.height + 1 == height => ng.reward }.flatten
    }
  }

  override def lastBlockReward: Option[Long] = readLock {
    ngState.flatMap(_.reward) orElse blockchain.lastBlockReward
  }

  override def blockRewardVotes(height: Int): Seq[Long] = readLock {
    activatedFeatures.get(BlockchainFeatures.BlockReward.id) match {
      case Some(activatedAt) if activatedAt <= height =>
        ngState match {
          case None => blockchain.blockRewardVotes(height)
          case Some(ng) =>
            val innerVotes = blockchain.blockRewardVotes(height)
            if (height == this.height && settings.rewardsSettings.votingWindow(activatedAt, height).contains(height))
              innerVotes :+ ng.base.header.rewardVote
            else innerVotes
        }
      case None => Seq()
    }
  }

  override def wavesAmount(height: Int): BigInt = readLock {
    ngState match {
      case Some(ng) if this.height == height =>
        blockchain.wavesAmount(height - 1) + ng.reward.map(BigInt(_)).getOrElse(BigInt(0))
      case _ => blockchain.wavesAmount(height)
    }
  }

  private def liquidBlockHeaderAndSize(): Option[BlockInfo] = ngState.map { s =>
    BlockInfo(s.bestLiquidBlock.header, s.bestLiquidBlock.bytes().length, s.bestLiquidBlock.transactionData.size, s.bestLiquidBlock.signature)
  }

  override def blockInfo(blockId: BlockId): Option[BlockInfo] = readLock {
    liquidBlockHeaderAndSize().filter(_.signature == blockId) orElse blockchain.blockInfo(blockId)
  }

  override def height: Int = readLock {
    blockchain.height + ngState.fold(0)(_ => 1)
  }

  override def blockBytes(height: Int): Option[Array[Byte]] = readLock {
    compositeBlockchain.blockBytes(height)
  }

  override def heightOf(blockId: BlockId): Option[Int] = readLock {
    compositeBlockchain.heightOf(blockId)
  }

  override def lastBlockIds(howMany: Int): Seq[BlockId] = readLock {
    ngState.fold(blockchain.lastBlockIds(howMany))(_.bestLiquidBlockId +: blockchain.lastBlockIds(howMany - 1))
  }

  override def microBlock(id: BlockId): Option[MicroBlock] = readLock {
    for {
      ng <- ngState
      mb <- ng.microBlock(id)
    } yield mb
  }

  def lastBlockTimestamp: Option[Long] = readLock {
    ngState.map(_.base.header.timestamp).orElse(blockchain.lastBlockTimestamp)
  }

  def lastBlockId: Option[ByteStr] = readLock {
    ngState.map(_.bestLiquidBlockId).orElse(blockchain.lastBlockId)
  }

  def blockAt(height: Int): Option[Block] = readLock {
    if (height == this.height)
      ngState.map(_.bestLiquidBlock)
    else
      blockchain.blockAt(height)
  }

  override def lastPersistedBlockIds(count: Int): Seq[BlockId] = readLock {
    blockchain.lastBlockIds(count)
  }

  override def microblockIds: Seq[BlockId] = readLock {
    ngState.fold(Seq.empty[BlockId])(_.microBlockIds)
  }

  override def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo] = readLock {
    ngState
      .map(_.bestLastBlockInfo(maxTimestamp))
      .orElse(
        blockchain.lastBlock.map(
          b => BlockMinerInfo(NxtLikeConsensusBlockData(b.header.baseTarget, b.header.generationSignature), b.header.timestamp, b.uniqueId)
        )
      )
  }

  override def score: BigInt = readLock {
    blockchain.score + ngState.fold(BigInt(0))(_.bestLiquidBlock.blockScore())
  }

  override def lastBlock: Option[Block] = readLock {
    ngState.map(_.bestLiquidBlock).orElse(blockchain.lastBlock)
  }

  override def carryFee: Long = readLock {
    ngState.map(_.carryFee).getOrElse(blockchain.carryFee)
  }

  override def blockBytes(blockId: ByteStr): Option[Array[Byte]] = readLock {
    (for {
      ng                  <- ngState
      (block, _, _, _, _) <- ng.totalDiffOf(blockId)
    } yield block.bytes()).orElse(blockchain.blockBytes(blockId))
  }

  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = readLock {
    ngState match {
      case Some(ng) if ng.contains(parentSignature) => Some(Seq.empty[ByteStr])
      case maybeNg =>
        blockchain.blockIdsAfter(parentSignature, howMany).map { ib =>
          if (ib.lengthCompare(howMany) < 0) ib ++ maybeNg.map(_.bestLiquidBlockId) else ib
        }
    }
  }

  override def parentHeader(block: BlockHeader, back: Int): Option[BlockHeader] = readLock {
    ngState match {
      case Some(ng) if ng.contains(block.reference) =>
        if (back == 1) Some(ng.base.header) else blockchain.parentHeader(ng.base.header, back - 1)
      case _ =>
        blockchain.parentHeader(block, back)
    }
  }

  override def totalFee(height: Int): Option[Long] = readLock {
    if (height == this.height)
      ngState.map(_.bestLiquidDiffAndFees._3)
    else
      blockchain.totalFee(height)
  }

  override def blockInfo(height: Int): Option[BlockInfo] = readLock {
    compositeBlockchain.blockInfo(height)
  }

  override def transferById(id: BlockId): Option[(Int, TransferTransaction)] = readLock {
    compositeBlockchain.transferById(id)
  }

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] = readLock {
    compositeBlockchain.transactionInfo(id)
  }

  override def containsTransaction(tx: Transaction): Boolean = readLock {
    compositeBlockchain.containsTransaction(tx)
  }

  override def assetDescription(id: IssuedAsset): Option[AssetDescription] = readLock {
    compositeBlockchain.assetDescription(id)
  }

  override def resolveAlias(alias: Alias): Either[ValidationError, Address] = readLock {
    compositeBlockchain.resolveAlias(alias)
  }

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = readLock {
    compositeBlockchain.leaseDetails(leaseId)
  }

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = readLock {
    compositeBlockchain.filledVolumeAndFee(orderId)
  }

  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  override def balanceOnlySnapshots(address: Address, h: Int, assetId: Asset = Waves): Option[(Int, Long)] = readLock {
    CompositeBlockchain(blockchain, ngState.map(_.bestLiquidDiff)).balanceOnlySnapshots(address, h, assetId)
  }

  override def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot] = readLock {
    CompositeBlockchain(blockchain, ngState.map(_.bestLiquidDiff))
      .balanceSnapshots(address, from, to)
  }

  override def accountScriptWithComplexity(address: Address): Option[(PublicKey, Script, Long, Map[String, Long])] = readLock {
    compositeBlockchain.accountScriptWithComplexity(address)
  }

  override def hasScript(address: Address): Boolean = readLock {
    compositeBlockchain.hasScript(address)
  }

  override def assetScriptWithComplexity(asset: IssuedAsset): Option[(PublicKey, Script, Long)] = readLock {
    compositeBlockchain.assetScriptWithComplexity(asset)
  }

  override def hasAssetScript(asset: IssuedAsset): Boolean = readLock {
    compositeBlockchain.hasAssetScript(asset)
  }

  override def accountDataKeys(address: Address): Set[String] = {
    compositeBlockchain.accountDataKeys(address)
  }

  override def accountData(acc: Address): AccountDataInfo = readLock {
    compositeBlockchain.accountData(acc)
  }

  override def accountData(acc: Address, key: String): Option[DataEntry[_]] = readLock {
    compositeBlockchain.accountData(acc, key)
  }

  def collectActiveLeases(from: Int, to: Int)(filter: LeaseTransaction => Boolean): Seq[LeaseTransaction] =
    CompositeBlockchain.collectActiveLeases(blockchain, ngState.map(_.bestLiquidDiff), height, from, to)(filter)

  /** Builds a new portfolio map by applying a partial function to all portfolios on which the function is defined.
    *
    * @note Portfolios passed to `pf` only contain Waves and Leasing balances to improve performance */
  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = readLock {
    ngState.fold(blockchain.collectLposPortfolios(pf)) { ng =>
      val b = Map.newBuilder[Address, A]
      for ((a, p) <- ng.bestLiquidDiff.portfolios if p.lease != LeaseBalance.empty || p.balance != 0) {
        pf.runWith(b += a -> _)(a -> this.wavesPortfolio(a))
      }

      blockchain.collectLposPortfolios(pf) ++ b.result()
    }
  }

  override def invokeScriptResult(txId: TransactionId): Either[ValidationError, InvokeScriptResult] = readLock {
    compositeBlockchain.invokeScriptResult(txId)
  }

  override def transactionHeight(id: ByteStr): Option[Int] = readLock {
    compositeBlockchain.transactionHeight(id)
  }

  override def balance(address: Address, mayBeAssetId: Asset): Long = readLock {
    compositeBlockchain.balance(address, mayBeAssetId)
  }

  override def leaseBalance(address: Address): LeaseBalance = readLock {
    compositeBlockchain.leaseBalance(address)
  }

  override def hitSourceAtHeight(height: Int): Option[ByteStr] = readLock {
    ngState match {
      case Some(ng) if this.height == height => ng.hitSource.some
      case _                                 => blockchain.hitSourceAtHeight(height)
    }
  }

  private[this] def compositeBlockchain = {
    ngState.fold(blockchain: Blockchain)(CompositeBlockchain(blockchain, _))
  }

  private[this] object metrics {
    val blockMicroForkStats       = Kamon.counter("blockchain-updater.block-micro-fork")
    val microMicroForkStats       = Kamon.counter("blockchain-updater.micro-micro-fork")
    val microBlockForkStats       = Kamon.counter("blockchain-updater.micro-block-fork")
    val microBlockForkHeightStats = Kamon.histogram("blockchain-updater.micro-block-fork-height")
    val forgeBlockTimeStats       = Kamon.timer("blockchain-updater.forge-block-time")
  }
}

object BlockchainUpdaterImpl
    extends ScorexLogging
    with AddressTransactions.Prov[BlockchainUpdaterImpl]
    with Distributions.Prov[BlockchainUpdaterImpl] {
  def areVersionsOfSameBlock(b1: Block, b2: Block): Boolean =
    b1.header.generator == b2.header.generator &&
      b1.header.baseTarget == b2.header.baseTarget &&
      b1.header.reference == b2.header.reference &&
      b1.header.timestamp == b2.header.timestamp

  def addressTransactions(bu: BlockchainUpdaterImpl): AddressTransactions =
    new CompositeAddressTransactions(bu.blockchain, Height @@ bu.height, () => bu.bestLiquidDiff)

  def distributions(bu: BlockchainUpdaterImpl): Distributions =
    new CompositeDistributions(bu, bu.blockchain, () => bu.bestLiquidDiff)
}
