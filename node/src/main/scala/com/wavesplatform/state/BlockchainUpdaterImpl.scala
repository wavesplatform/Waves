package com.wavesplatform.state

import cats.syntax.either.*
import cats.syntax.option.*
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.RocksDBWriter
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.ConsensusImprovements
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.{TxsInBlockchainStats, *}
import com.wavesplatform.mining.{Miner, MiningConstraint, MiningConstraints}
import com.wavesplatform.settings.{BlockchainSettings, WavesSettings}
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.reader.{CompositeBlockchain, LeaseDetails}
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{BlockAppendError, GenericError, MicroBlockAppendError}
import com.wavesplatform.transaction.lease.*
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.utils.{ScorexLogging, Time, UnsupportedFeature, forceStopApplication}
import kamon.Kamon
import monix.reactive.subjects.ReplaySubject
import monix.reactive.Observable

import java.util.concurrent.locks.{Lock, ReentrantReadWriteLock}

class BlockchainUpdaterImpl(
    rocksdb: RocksDBWriter,
    wavesSettings: WavesSettings,
    time: Time,
    blockchainUpdateTriggers: BlockchainUpdateTriggers,
    collectActiveLeases: (Int, Int) => Seq[LeaseTransaction],
    miner: Miner = _ => ()
) extends Blockchain
    with BlockchainUpdater
    with NG
    with ScorexLogging {

  import com.wavesplatform.state.BlockchainUpdaterImpl.*
  import wavesSettings.blockchainSettings.functionalitySettings

  private def inLock[R](l: Lock, f: => R): R = {
    l.lockInterruptibly()
    try f
    finally l.unlock()
  }

  private val lock                     = new ReentrantReadWriteLock(true)
  private def writeLock[B](f: => B): B = inLock(lock.writeLock(), f)
  def readLock[B](f: => B): B          = inLock(lock.readLock(), f)

  private lazy val maxBlockReadinessAge = wavesSettings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed.toMillis

  @volatile
  private[this] var ngState: Option[NgState] = Option.empty

  @volatile
  private[this] var restTotalConstraint: MiningConstraint = MiningConstraints(rocksdb, rocksdb.height).total

  private val internalLastBlockInfo = ReplaySubject.createLimited[LastBlockInfo](1)

  private def lastBlockReward: Option[Long] = this.blockReward(this.height)

  private def publishLastBlockInfo(): Unit =
    for (id <- this.lastBlockId; ts <- ngState.map(_.base.header.timestamp).orElse(rocksdb.lastBlockTimestamp)) {
      val blockchainReady = ts + maxBlockReadinessAge > time.correctedTime()
      internalLastBlockInfo.onNext(LastBlockInfo(id, height, score, blockchainReady))
    }

  publishLastBlockInfo()

  def liquidBlock(id: ByteStr): Option[Block] = readLock(ngState.flatMap(_.totalDiffOf(id).map(_._1)))

  def liquidTransactions(id: ByteStr): Option[Seq[(TxMeta, Transaction)]] =
    readLock(
      ngState
        .flatMap(_.totalDiffOf(id))
        .map { case (_, diff, _, _, _) =>
          diff.transactions.toSeq.map(info => (TxMeta(Height(height), info.status, info.spentComplexity), info.transaction))
        }
    )

  def liquidBlockMeta: Option[BlockMeta] =
    readLock(ngState.map { ng =>
      val (_, _, totalFee) = ng.bestLiquidDiffAndFees
      val b                = ng.bestLiquidBlock
      val vrf              = if (b.header.version >= Block.ProtoBlockVersion) hitSource(height) else None
      BlockMeta.fromBlock(b, height, totalFee, ng.reward, vrf)
    })

  @noinline
  def bestLiquidDiff: Option[Diff] = readLock(ngState.map(_.bestLiquidDiff))

  def bestLiquidDiffAndFees: Option[(Diff, Long, Long)] = readLock(ngState.map(_.bestLiquidDiffAndFees))

  override val settings: BlockchainSettings = wavesSettings.blockchainSettings

  override def isLastBlockId(id: ByteStr): Boolean = readLock {
    ngState.fold(rocksdb.lastBlockId.contains(id))(_.contains(id))
  }

  override val lastBlockInfo: Observable[LastBlockInfo] = internalLastBlockInfo

  private def featuresApprovedWithBlock(block: Block): Set[Short] = {
    val height = rocksdb.height + 1

    val featuresCheckPeriod        = functionalitySettings.activationWindowSize(height)
    val blocksForFeatureActivation = functionalitySettings.blocksForFeatureActivation(height)

    if (height % featuresCheckPeriod == 0) {
      val approvedFeatures = rocksdb
        .featureVotes(height)
        .map { case (feature, votes) => feature -> (if (block.header.featureVotes.contains(feature)) votes + 1 else votes) }
        .filter { case (_, votes) => votes >= blocksForFeatureActivation }
        .keySet
        .filterNot(settings.functionalitySettings.preActivatedFeatures.contains)

      if (approvedFeatures.nonEmpty) log.info(s"${displayFeatures(approvedFeatures)} APPROVED at height $height")

      val unimplementedApproved = approvedFeatures.diff(BlockchainFeatures.implemented)
      if (unimplementedApproved.nonEmpty) {
        log.warn(s"""UNIMPLEMENTED ${displayFeatures(unimplementedApproved)} APPROVED ON BLOCKCHAIN
                    |PLEASE, UPDATE THE NODE AS SOON AS POSSIBLE
                    |OTHERWISE THE NODE WILL BE STOPPED OR FORKED UPON FEATURE ACTIVATION""".stripMargin)
      }

      val activatedFeatures: Set[Short] = rocksdb.activatedFeaturesAt(height)

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

  def computeNextReward: Option[Long] = {
    val settings   = this.settings.rewardsSettings
    val nextHeight = this.height + 1

    if (height == 0 && rocksdb.featureActivationHeight(ConsensusImprovements.id).exists(_ <= 1))
      None
    else
      rocksdb
        .featureActivationHeight(BlockchainFeatures.BlockReward.id)
        .filter(_ <= nextHeight)
        .flatMap { activatedAt =>
          val mayBeReward     = lastBlockReward
          val mayBeTimeToVote = nextHeight - activatedAt
          val modifiedTerm = if (rocksdb.isFeatureActivated(BlockchainFeatures.CappedReward, this.height)) {
            settings.termAfterCappedRewardFeature
          } else {
            settings.term
          }

          mayBeReward match {
            case Some(reward) if mayBeTimeToVote > 0 && mayBeTimeToVote % modifiedTerm == 0 =>
              Some((blockRewardVotes(this.height).filter(_ >= 0), reward))
            case None if mayBeTimeToVote >= 0 =>
              Some((Seq(), settings.initial))
            case _ => None
          }
        }
        .flatMap { case (votes, currentReward) =>
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

  override def processBlock(
      block: Block,
      hitSource: ByteStr,
      challengedHitSource: Option[ByteStr] = None,
      verify: Boolean = true,
      txSignParCheck: Boolean = true,
      checkStateHash: Boolean = true // TODO: remove after NODE-2568 merge
  ): Either[ValidationError, Seq[Diff]] =
    writeLock {
      val height                             = rocksdb.height
      val notImplementedFeatures: Set[Short] = rocksdb.activatedFeaturesAt(height).diff(BlockchainFeatures.implemented)

      Either
        .cond(
          !wavesSettings.featuresSettings.autoShutdownOnUnsupportedFeature || notImplementedFeatures.isEmpty,
          (),
          GenericError(s"UNIMPLEMENTED ${displayFeatures(notImplementedFeatures)} ACTIVATED ON BLOCKCHAIN, UPDATE THE NODE IMMEDIATELY")
        )
        .flatMap[ValidationError, Seq[Diff]](_ =>
          (ngState match {
            case None =>
              rocksdb.lastBlockId match {
                case Some(uniqueId) if uniqueId != block.header.reference =>
                  val logDetails = s"The referenced block(${block.header.reference})" +
                    s" ${if (rocksdb.contains(block.header.reference)) "exists, it's not last persisted" else "doesn't exist"}"
                  Left(BlockAppendError(s"References incorrect or non-existing block: " + logDetails, block))
                case lastBlockId =>
                  val height            = lastBlockId.fold(0)(rocksdb.unsafeHeightOf)
                  val miningConstraints = MiningConstraints(rocksdb, height)
                  val reward            = computeNextReward

                  val referencedBlockchain = CompositeBlockchain(rocksdb, reward)
                  BlockDiffer
                    .fromBlock(
                      referencedBlockchain,
                      rocksdb.lastBlock,
                      block,
                      miningConstraints.total,
                      hitSource,
                      challengedHitSource,
                      rocksdb.loadCacheData,
                      verify,
                      txSignParCheck = txSignParCheck,
                      checkStateHash = checkStateHash
                    )
                    .map { r =>
                      val updatedBlockchain = CompositeBlockchain(rocksdb, r.diff, block, hitSource, r.carry, reward)
                      miner.scheduleMining(Some(updatedBlockchain))
                      blockchainUpdateTriggers.onProcessBlock(block, r.detailedDiff, reward, hitSource, referencedBlockchain)
                      Option((r, Nil, reward, hitSource))
                    }
              }
            case Some(ng) =>
              if (ng.base.header.reference == block.header.reference) {
                if (block.header.timestamp < ng.base.header.timestamp) {
                  val height            = rocksdb.unsafeHeightOf(ng.base.header.reference)
                  val miningConstraints = MiningConstraints(rocksdb, height)

                  blockchainUpdateTriggers.onRollback(this, ng.base.header.reference, rocksdb.height)

                  val referencedBlockchain = CompositeBlockchain(rocksdb, ng.reward)
                  BlockDiffer
                    .fromBlock(
                      referencedBlockchain,
                      rocksdb.lastBlock,
                      block,
                      miningConstraints.total,
                      hitSource,
                      challengedHitSource,
                      rocksdb.loadCacheData,
                      verify,
                      txSignParCheck = txSignParCheck,
                      checkStateHash = checkStateHash
                    )
                    .map { r =>
                      log.trace(
                        s"Better liquid block(timestamp=${block.header.timestamp}) received and applied instead of existing(timestamp=${ng.base.header.timestamp})"
                      )
                      val (mbs, diffs) = ng.allDiffs.unzip
                      log.trace(s"Discarded microblocks = $mbs, diffs = ${diffs.map(_.hashString)}")
                      blockchainUpdateTriggers.onProcessBlock(block, r.detailedDiff, ng.reward, hitSource, referencedBlockchain)
                      Some((r, diffs, ng.reward, hitSource))
                    }
                } else if (areVersionsOfSameBlock(block, ng.base)) {
                  if (block.transactionData.lengthCompare(ng.transactions.size) <= 0) {
                    log.trace(s"Existing liquid block is better than new one, discarding $block")
                    Right(None)
                  } else {
                    log.trace(s"New liquid block is better version of existing, swapping")
                    val height            = rocksdb.unsafeHeightOf(ng.base.header.reference)
                    val miningConstraints = MiningConstraints(rocksdb, height)

                    blockchainUpdateTriggers.onRollback(this, ng.base.header.reference, rocksdb.height)

                    val referencedBlockchain = CompositeBlockchain(rocksdb, ng.reward)
                    BlockDiffer
                      .fromBlock(
                        referencedBlockchain,
                        rocksdb.lastBlock,
                        block,
                        miningConstraints.total,
                        hitSource,
                        challengedHitSource,
                        rocksdb.loadCacheData,
                        verify,
                        txSignParCheck = txSignParCheck,
                        checkStateHash = checkStateHash
                      )
                      .map { r =>
                        blockchainUpdateTriggers.onProcessBlock(block, r.detailedDiff, ng.reward, hitSource, referencedBlockchain)
                        Some((r, Nil, ng.reward, hitSource))
                      }
                  }
                } else
                  Left(
                    BlockAppendError(
                      s"Competitors liquid block $block(timestamp=${block.header.timestamp}) is not better than existing (ng.base ${ng.base}(timestamp=${ng.base.header.timestamp}))",
                      block
                    )
                  )
              } else
                metrics.forgeBlockTimeStats.measureOptional(ng.totalDiffOf(block.header.reference)) match {
                  case None => Left(BlockAppendError(s"References incorrect or non-existing block", block))
                  case Some((referencedForgedBlock, referencedLiquidDiff, carry, totalFee, discarded)) =>
                    if (!verify || referencedForgedBlock.signatureValid()) {
                      val height = rocksdb.heightOf(referencedForgedBlock.header.reference).getOrElse(0)

                      if (discarded.nonEmpty) {
                        blockchainUpdateTriggers.onMicroBlockRollback(this, block.header.reference)
                        metrics.microBlockForkStats.increment()
                        metrics.microBlockForkHeightStats.record(discarded.size)
                      }

                      val constraint: MiningConstraint = {
                        val miningConstraints = MiningConstraints(rocksdb, height)
                        miningConstraints.total
                      }

                      val prevReward = ng.reward
                      val reward     = computeNextReward

                      val prevHitSource = ng.hitSource

                      for {
                        liquidDiffWithCancelledLeases <- ng.cancelExpiredLeases(referencedLiquidDiff).leftMap(GenericError(_))
                        referencedBlockchain = CompositeBlockchain(
                          rocksdb,
                          liquidDiffWithCancelledLeases,
                          referencedForgedBlock,
                          ng.hitSource,
                          carry,
                          reward
                        )
                        differResult <- BlockDiffer
                          .fromBlock(
                            referencedBlockchain,
                            Some(referencedForgedBlock),
                            block,
                            constraint,
                            hitSource,
                            challengedHitSource,
                            rocksdb.loadCacheData,
                            verify,
                            txSignParCheck = txSignParCheck,
                            checkStateHash = checkStateHash
                          )
                      } yield {
                        val tempBlockchain = CompositeBlockchain(
                          referencedBlockchain,
                          differResult.diff,
                          block,
                          hitSource,
                          differResult.carry,
                          None
                        )
                        miner.scheduleMining(Some(tempBlockchain))

                        blockchainUpdateTriggers.onProcessBlock(block, differResult.detailedDiff, reward, hitSource, this)

                        rocksdb.append(liquidDiffWithCancelledLeases, carry, totalFee, prevReward, prevHitSource, referencedForgedBlock)
                        BlockStats.appended(referencedForgedBlock, referencedLiquidDiff.scriptsComplexity)
                        TxsInBlockchainStats.record(ng.transactions.size)
                        val (discardedMbs, discardedDiffs) = discarded.unzip
                        if (discardedMbs.nonEmpty) {
                          log.trace(s"Discarded microblocks: $discardedMbs")
                        }

                        Some((differResult, discardedDiffs, reward, hitSource))
                      }
                    } else {
                      val errorText = s"Forged block has invalid signature. Base: ${ng.base}, requested reference: ${block.header.reference}"
                      log.error(errorText)
                      Left(BlockAppendError(errorText, block))
                    }
                }
          }).map {
            _ map { case (BlockDiffer.Result(newBlockDiff, carry, totalFee, updatedTotalConstraint, _, _), discDiffs, reward, hitSource) =>
              val newHeight = rocksdb.height + 1

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
                  cancelLeases(collectLeasesToCancel(newHeight), newHeight)
                )
              )
              publishLastBlockInfo()

              if (
                (block.header.timestamp > time
                  .getTimestamp() - wavesSettings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed.toMillis) || (newHeight % 100 == 0)
              ) {
                log.info(s"New height: $newHeight")
              }

              discDiffs
            } getOrElse Nil
          }
        )
    }

  private def collectLeasesToCancel(newHeight: Int): Seq[LeaseTransaction] =
    if (rocksdb.isFeatureActivated(BlockchainFeatures.LeaseExpiration, newHeight)) {
      val toHeight = newHeight - rocksdb.settings.functionalitySettings.leaseExpiration
      val fromHeight = rocksdb.featureActivationHeight(BlockchainFeatures.LeaseExpiration.id) match {
        case Some(`newHeight`) =>
          log.trace(s"Collecting leases created up till height $toHeight")
          1
        case _ =>
          log.trace(s"Collecting leases created at height $toHeight")
          toHeight
      }
      collectActiveLeases(fromHeight, toHeight)
    } else Seq.empty

  private def cancelLeases(leaseTransactions: Seq[LeaseTransaction], height: Int): Map[ByteStr, Diff] =
    (for {
      lt        <- leaseTransactions
      ltMeta    <- transactionMeta(lt.id()).toSeq
      recipient <- rocksdb.resolveAlias(lt.recipient).toSeq
    } yield lt.id() -> Diff(
      portfolios = Map(
        lt.sender.toAddress -> Portfolio(0, LeaseBalance(0, -lt.amount.value)),
        recipient           -> Portfolio(0, LeaseBalance(-lt.amount.value, 0))
      ),
      leaseState = Map((lt.id(), LeaseDetails(lt.sender, lt.recipient, lt.amount.value, LeaseDetails.Status.Expired(height), lt.id(), ltMeta.height)))
    )).toMap

  override def removeAfter(blockId: ByteStr): Either[ValidationError, Seq[(Block, ByteStr)]] = writeLock {
    log.info(s"Trying rollback blockchain to $blockId")

    val prevNgState = ngState

    val result = prevNgState match {
      case Some(ng) if ng.contains(blockId) =>
        log.trace("Resetting liquid block, no rollback necessary")
        Right(Seq.empty)
      case maybeNg =>
        for {
          height <- rocksdb.heightOf(blockId).toRight(GenericError(s"No such block $blockId"))
          _ <- Either.cond(
            height >= rocksdb.safeRollbackHeight,
            (),
            GenericError(s"Rollback is possible only to the block at the height ${rocksdb.safeRollbackHeight}")
          )
          _ = blockchainUpdateTriggers.onRollback(this, blockId, height)
          blocks <- rocksdb.rollbackTo(height).leftMap(GenericError(_))
        } yield {
          ngState = None
          blocks ++ maybeNg.map(ng => (ng.bestLiquidBlock, ng.hitSource)).toSeq
        }
    }

    result match {
      case Right(_) =>
        log.info(s"Blockchain rollback to $blockId succeeded")
        publishLastBlockInfo()
        miner.scheduleMining()

      case Left(error) =>
        log.error(s"Blockchain rollback to $blockId failed: ${error.err}")
    }
    result
  }

  override def processMicroBlock(microBlock: MicroBlock, verify: Boolean = true): Either[ValidationError, BlockId] =
    writeLock {
      ngState match {
        case None =>
          Left(MicroBlockAppendError("No base block exists", microBlock))
        case Some(ng) if ng.base.header.generator.toAddress != microBlock.sender.toAddress =>
          Left(MicroBlockAppendError("Base block has been generated by another account", microBlock))
        case Some(ng) if ng.base.header.challengedHeader.nonEmpty =>
          Left(MicroBlockAppendError("Base block has challenged header", microBlock))
        case Some(ng) =>
          ng.lastMicroBlock match {
            case None if ng.base.id() != microBlock.reference =>
              metrics.blockMicroForkStats.increment()
              Left(MicroBlockAppendError("It's first micro and it doesn't reference base block(which exists)", microBlock))
            case Some(_) if ng.bestLiquidBlockId != microBlock.reference =>
              metrics.microMicroForkStats.increment()
              Left(MicroBlockAppendError("It doesn't reference last known microBlock(which exists)", microBlock))
            case _ =>
              for {
                _ <- microBlock.signaturesValid()
                (totalSignatureValid, prevStateHash) <- ng
                  .totalDiffOf(microBlock.reference)
                  .toRight(GenericError(s"No referenced block exists: $microBlock"))
                  .map { case (accumulatedBlock, _, _, _, _) =>
                    Block
                      .create(
                        accumulatedBlock,
                        accumulatedBlock.transactionData ++ microBlock.transactionData,
                        microBlock.totalResBlockSig,
                        microBlock.stateHash
                      )
                      .signatureValid() -> accumulatedBlock.header.stateHash
                  }
                _ <- Either
                  .cond(
                    totalSignatureValid,
                    (),
                    MicroBlockAppendError("Invalid total block signature", microBlock)
                  )
                blockDifferResult <- {
                  BlockDiffer.fromMicroBlock(
                    this,
                    rocksdb.lastBlockTimestamp,
                    prevStateHash,
                    microBlock,
                    restTotalConstraint,
                    rocksdb.loadCacheData,
                    verify
                  )
                }
              } yield {
                val BlockDiffer.Result(diff, carry, totalFee, updatedMdConstraint, detailedDiff, _) = blockDifferResult
                restTotalConstraint = updatedMdConstraint
                val blockId = ng.createBlockId(microBlock)

                val transactionsRoot = ng.createTransactionsRoot(microBlock)
                blockchainUpdateTriggers.onProcessMicroBlock(microBlock, detailedDiff, this, blockId, transactionsRoot)

                this.ngState = Some(ng.append(microBlock, diff, carry, totalFee, System.currentTimeMillis, Some(blockId)))

                log.info(s"${microBlock.stringRepr(blockId)} appended, diff=${diff.hashString}")
                internalLastBlockInfo.onNext(LastBlockInfo(blockId, height, score, ready = true))

                blockId
              }
          }
      }
    }

  def shutdown(): Unit = {
    internalLastBlockInfo.onComplete()
  }

  private def newlyApprovedFeatures = ngState.fold(Map.empty[Short, Int])(_.approvedFeatures.map(_ -> height).toMap)

  override def approvedFeatures: Map[Short, Int] = readLock {
    newlyApprovedFeatures ++ rocksdb.approvedFeatures
  }

  override def activatedFeatures: Map[Short, Int] = readLock {
    (newlyApprovedFeatures.view.mapValues(_ + functionalitySettings.activationWindowSize(height)) ++ rocksdb.activatedFeatures).toMap
  }

  override def featureVotes(height: Int): Map[Short, Int] = readLock {
    val innerVotes = rocksdb.featureVotes(height)
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
    rocksdb.blockReward(height) match {
      case r @ Some(_) => r
      case None        => ngState.collect { case ng if rocksdb.height + 1 == height => ng.reward }.flatten
    }
  }

  override def blockRewardVotes(height: Int): Seq[Long] = readLock {
    activatedFeatures.get(BlockchainFeatures.BlockReward.id) match {
      case Some(activatedAt) if activatedAt <= height =>
        ngState match {
          case None => rocksdb.blockRewardVotes(height)
          case Some(ng) =>
            val innerVotes = rocksdb.blockRewardVotes(height)
            val modifyTerm = activatedFeatures.get(BlockchainFeatures.CappedReward.id).exists(_ <= height)
            if (height == this.height && settings.rewardsSettings.votingWindow(activatedAt, height, modifyTerm).contains(height))
              innerVotes :+ ng.base.header.rewardVote
            else innerVotes
        }
      case _ => Seq()
    }
  }

  override def wavesAmount(height: Int): BigInt = readLock {
    ngState match {
      case Some(ng) if this.height == height =>
        rocksdb.wavesAmount(height - 1) + BigInt(ng.reward.getOrElse(0L))
      case _ =>
        rocksdb.wavesAmount(height)
    }
  }

  override def height: Int = readLock {
    rocksdb.height + ngState.fold(0)(_ => 1)
  }

  override def heightOf(blockId: BlockId): Option[Int] = readLock {
    ngState
      .collect {
        case ng if ng.contains(blockId) => this.height
      }
      .orElse(rocksdb.heightOf(blockId))
  }

  override def microBlock(id: BlockId): Option[MicroBlock] = readLock {
    for {
      ng <- ngState
      mb <- ng.microBlock(id)
    } yield mb
  }

  override def microblockIds: Seq[BlockId] = readLock {
    ngState.fold(Seq.empty[BlockId])(_.microBlockIds)
  }

  override def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo] = readLock {
    ngState
      .map(_.bestLastBlockInfo(maxTimestamp))
      .orElse(
        rocksdb.lastBlockHeader.map { sh =>
          BlockMinerInfo(sh.header.baseTarget, sh.header.generationSignature, sh.header.timestamp, sh.id())
        }
      )
  }

  override def score: BigInt = readLock {
    rocksdb.score + ngState.fold(BigInt(0))(_.bestLiquidBlock.blockScore())
  }

  override def carryFee: Long = readLock {
    ngState.fold(rocksdb.carryFee)(_.carryFee)
  }

  override def blockHeader(height: Int): Option[SignedBlockHeader] = readLock {
    if (height == rocksdb.height + 1) ngState.map { x =>
      SignedBlockHeader(x.bestLiquidBlock.header, x.bestLiquidBlock.signature)
    }
    else rocksdb.blockHeader(height)
  }

  override def transferById(id: BlockId): Option[(Int, TransferTransactionLike)] = readLock {
    compositeBlockchain.transferById(id)
  }

  override def transactionInfo(id: ByteStr): Option[(TxMeta, Transaction)] = readLock {
    compositeBlockchain.transactionInfo(id)
  }

  override def transactionInfos(ids: Seq[BlockId]): Seq[Option[(TxMeta, Transaction)]] = readLock {
    compositeBlockchain.transactionInfos(ids)
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
  override def balanceAtHeight(address: Address, h: Int, assetId: Asset = Waves): Option[(Int, Long)] = readLock {
    compositeBlockchain.balanceAtHeight(address, h, assetId)
  }

  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] = readLock {
    to.fold(ngState.flatMap(ng => ng.totalDiffOf(ng.bestLiquidBlockId))) { id =>
      ngState.flatMap(_.totalDiffOf(id))
    }.fold[Blockchain](rocksdb) { case (block, diff, _, _, _) =>
      CompositeBlockchain(rocksdb, diff, block, ByteStr.empty, 0L, None)
    }.balanceSnapshots(address, from, to)
  }

  override def accountScript(address: Address): Option[AccountScriptInfo] = readLock {
    compositeBlockchain.accountScript(address)
  }

  override def hasAccountScript(address: Address): Boolean = readLock {
    compositeBlockchain.hasAccountScript(address)
  }

  override def assetScript(asset: IssuedAsset): Option[AssetScriptInfo] = readLock {
    compositeBlockchain.assetScript(asset)
  }

  override def accountData(acc: Address, key: String): Option[DataEntry[?]] = readLock {
    compositeBlockchain.accountData(acc, key)
  }

  override def hasData(acc: Address): Boolean = readLock {
    compositeBlockchain.hasData(acc)
  }

  override def transactionMeta(id: ByteStr): Option[TxMeta] = readLock {
    compositeBlockchain.transactionMeta(id)
  }

  override def balance(address: Address, mayBeAssetId: Asset): Long = readLock {
    compositeBlockchain.balance(address, mayBeAssetId)
  }

  override def balances(req: Seq[(Address, Asset)]): Map[(Address, Asset), TxTimestamp] = readLock {
    compositeBlockchain.balances(req)
  }

  override def wavesBalances(addresses: Seq[Address]): Map[Address, Long] = readLock {
    compositeBlockchain.wavesBalances(addresses)
  }

  override def effectiveBalanceBanHeights(address: Address): Seq[Int] = readLock {
    compositeBlockchain.effectiveBalanceBanHeights(address)
  }

  override def leaseBalance(address: Address): LeaseBalance = readLock {
    compositeBlockchain.leaseBalance(address)
  }

  override def leaseBalances(addresses: Seq[Address]): Map[Address, LeaseBalance] = readLock {
    compositeBlockchain.leaseBalances(addresses)
  }

  override def hitSource(height: Int): Option[ByteStr] = readLock {
    ngState match {
      case Some(ng) if this.height == height => ng.hitSource.some
      case _                                 => rocksdb.hitSource(height)
    }
  }

  override def resolveERC20Address(address: ERC20Address): Option[IssuedAsset] = readLock {
    compositeBlockchain.resolveERC20Address(address)
  }

  def getCompositeBlockchain: CompositeBlockchain =
    ngState.fold(CompositeBlockchain(rocksdb, Diff.empty))(CompositeBlockchain(rocksdb, _))

  private[this] def compositeBlockchain: Blockchain =
    ngState.fold(rocksdb: Blockchain)(CompositeBlockchain(rocksdb, _))

  // noinspection ScalaStyle,TypeAnnotation
  private[this] object metrics {
    val blockMicroForkStats       = Kamon.counter("blockchain-updater.block-micro-fork").withoutTags()
    val microMicroForkStats       = Kamon.counter("blockchain-updater.micro-micro-fork").withoutTags()
    val microBlockForkStats       = Kamon.counter("blockchain-updater.micro-block-fork").withoutTags()
    val microBlockForkHeightStats = Kamon.histogram("blockchain-updater.micro-block-fork-height").withoutTags()
    val forgeBlockTimeStats       = Kamon.timer("blockchain-updater.forge-block-time").withoutTags()
  }
}

object BlockchainUpdaterImpl {
  private def displayFeatures(s: Set[Short]): String =
    s"FEATURE${if (s.size > 1) "S" else ""} ${s.mkString(", ")} ${if (s.size > 1) "have been" else "has been"}"

  def areVersionsOfSameBlock(b1: Block, b2: Block): Boolean =
    b1.header.generator == b2.header.generator &&
      b1.header.baseTarget == b2.header.baseTarget &&
      b1.header.reference == b2.header.reference &&
      b1.header.timestamp == b2.header.timestamp
}
