package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.data.{NonEmptyList => NEL}
import cats.implicits._
import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.metrics.{Instrumented, TxsInBlockchainStats}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.diffs.BlockDiffer
import com.wavesplatform.state2.reader.CompositeStateReader.composite
import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.utils.{UnsupportedFeature, forceStopApplication}
import kamon.Kamon
import kamon.metric.instrument.Time
import monix.eval.Coeval
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.ConcurrentSubject
import scorex.block.{Block, MicroBlock}
import scorex.transaction.ValidationError.{BlockAppendError, GenericError, MicroBlockAppendError}
import scorex.transaction._
import scorex.utils.ScorexLogging

import scala.collection.immutable

class BlockchainUpdaterImpl private(persisted: StateWriter with SnapshotStateReader,
                                    settings: WavesSettings,
                                    featureProvider: FeatureProvider,
                                    historyWriter: HistoryWriterImpl,
                                    val synchronizationToken: ReentrantReadWriteLock) extends BlockchainUpdater with BlockchainDebugInfo with ScorexLogging with Instrumented {

  import com.wavesplatform.state2.BlockchainUpdaterImpl._

  private lazy val maxTransactionsPerChunk = settings.blockchainSettings.maxTransactionsPerBlockDiff
  private lazy val minBlocksInMemory = settings.blockchainSettings.minBlocksInMemory
  private lazy val rebuildByBlocks = minBlocksInMemory

  private lazy val inMemDiffs: Synchronized[NEL[BlockDiff]] = Synchronized(NEL.one(BlockDiff.empty)) // fresh head
  private lazy val ngState: Synchronized[Option[NgState]] = Synchronized(Option.empty[NgState])

  override val lastBlockId: ConcurrentSubject[ByteStr, ByteStr] = ConcurrentSubject.publish[ByteStr]

  private val unsafeDiffByRange = BlockDiffer.unsafeDiffByRange(settings.blockchainSettings.functionalitySettings, featureProvider, historyWriter, maxTransactionsPerChunk) _

  private def heights(prefix: String): String = read { implicit l =>
    s"$prefix, total persisted blocks: ${historyWriter.height()}, [ in-memory: ${inMemDiffs().toList.mkString(" | ")} ] + persisted: h=${persisted.height}"
  }

  private def currentPersistedBlocksState: StateReader = read { implicit l => Coeval(composite(inMemDiffs(), persisted)) }

  def bestLiquidState: StateReader = read { implicit l => composite(Coeval(ngState().map(_.bestLiquidDiff).orEmpty), currentPersistedBlocksState) }

  def historyReader: NgHistory with DebugNgHistory with FeatureProvider = read { implicit l => new NgHistoryReader(() => ngState(), historyWriter, settings.blockchainSettings.functionalitySettings) }

  private def syncPersistedAndInMemory(): Unit = write { implicit l =>
    log.info(heights("State rebuild started"))

    val notPersisted = historyWriter.height() - persisted.height
    val inMemSize = Math.min(notPersisted, minBlocksInMemory)
    val persistUpTo = historyWriter.height() - inMemSize + 1

    val persistFrom = persisted.height + 1
    ranges(persistFrom, persistUpTo, rebuildByBlocks).foreach { case (_, last) =>
      val diffs = unsafeDiffByRange(persisted, last)
      log.debug(s"Diffs built for Range(${persisted.height + 1}, $last): $diffs")
      diffs.toList.reverse.foreach(persisted.applyBlockDiff)
    }

    inMemDiffs.set(unsafeDiffByRange(persisted, historyWriter.height() + 1))
    log.info(heights("State rebuild finished"))
  }

  private def displayFeatures(s: Set[Short]): String = s"FEATURE${if (s.size > 1) "S"} ${s.mkString(", ")} ${if (s.size > 1) "WERE" else "WAS"}"

  private def featuresApprovedWithBlock(block: Block): Set[Short] = {
    val height = historyWriter.height() + 1

    if (height % settings.blockchainSettings.functionalitySettings.featureCheckBlocksPeriod == 0) {

      val approvedFeatures = historyWriter.featureVotesCountWithinActivationWindow(height)
        .map { case (feature, votes) => feature -> (if (block.featureVotes.contains(feature)) votes + 1 else votes) }
        .filter { case (_, votes) => votes >= settings.blockchainSettings.functionalitySettings.blocksForFeatureActivation }
        .keySet

      log.info(s"${displayFeatures(approvedFeatures)} APPROVED ON BLOCKCHAIN")

      val unimplementedApproved = approvedFeatures.diff(BlockchainFeatures.implemented)
      if (unimplementedApproved.nonEmpty) {
        log.warn(s"UNIMPLEMENTED ${displayFeatures(unimplementedApproved)} APPROVED ON BLOCKCHAIN")
        log.warn("PLEASE, UPDATE THE NODE AS SOON AS POSSIBLE")
        log.warn("OTHERWISE THE NODE WILL BE STOPPED OR FORKED UPON FEATURE ACTIVATION")
      }

      val activatedFeatures = historyWriter.activatedFeatures(height)

      val unimplementedActivated = activatedFeatures.diff(BlockchainFeatures.implemented)
      if (unimplementedActivated.nonEmpty) {
        log.error(s"UNIMPLEMENTED ${displayFeatures(unimplementedActivated)} ACTIVATED ON BLOCKCHAIN")
        log.error("PLEASE, UPDATE THE NODE IMMEDIATELY")
        if (settings.featuresSettings.autoShutdownOnUnsupportedFeature) {
          log.error("FOR THIS REASON THE NODE WAS STOPPED AUTOMATICALLY")
          forceStopApplication(UnsupportedFeature)
        }
        else log.error("OTHERWISE THE NODE WILL END UP ON A FORK")
      }

      approvedFeatures
    }
    else Set.empty
  }

  override def processBlock(block: Block): Either[ValidationError, Option[DiscardedTransactions]] = write { implicit l =>
    val height = historyWriter.height()
    val notImplementedFeatures = featureProvider.activatedFeatures(height).diff(BlockchainFeatures.implemented)

    Either.cond(!settings.featuresSettings.autoShutdownOnUnsupportedFeature || notImplementedFeatures.isEmpty, (),
      GenericError(s"UNIMPLEMENTED ${displayFeatures(notImplementedFeatures)} ACTIVATED ON BLOCKCHAIN, UPDATE THE NODE IMMEDIATELY")).flatMap(_ =>
      (ngState() match {
        case None =>
          historyWriter.lastBlock match {
            case Some(lastInner) if lastInner.uniqueId != block.reference =>
              val logDetails = s"The referenced block(${block.reference})" +
                s" ${if (historyWriter.contains(block.reference)) "exits, it's not last persisted" else "doesn't exist"}"
              Left(BlockAppendError(s"References incorrect or non-existing block: " + logDetails, block))
            case _ => BlockDiffer.fromBlock(settings.blockchainSettings.functionalitySettings, featureProvider, currentPersistedBlocksState(), historyWriter.lastBlock, block).map(d => Some((d, Seq.empty[Transaction])))
          }
        case Some(ng) =>
          if (ng.base.reference == block.reference) {
            if (block.blockScore() > ng.base.blockScore()) {
              BlockDiffer.fromBlock(settings.blockchainSettings.functionalitySettings, featureProvider, currentPersistedBlocksState(), historyWriter.lastBlock, block).map { diff =>
                log.trace(s"Better liquid block(score=${block.blockScore()}) received and applied instead of existing(score=${ng.base.blockScore()})")
                Some((diff, ng.transactions))
              }
            } else if (areVersionsOfSameBlock(block, ng.base)) {
              if (block.transactionData.size <= ng.transactions.size) {
                log.trace(s"Existing liquid block is better than new one, discarding $block")
                Right(None)
              } else {
                log.trace(s"New liquid block is better version of exsting, swapping")
                BlockDiffer.fromBlock(settings.blockchainSettings.functionalitySettings, featureProvider, currentPersistedBlocksState(), historyWriter.lastBlock, block).map(d => Some((d, Seq.empty[Transaction])))
              }
            } else Left(BlockAppendError(s"Competitor's liquid block $block(score=${block.blockScore()}) is not better than existing (ng.base ${ng.base}(score=${ng.base.blockScore()}))", block))
          } else
            measureSuccessful(forgeBlockTimeStats, ng.totalDiffOf(block.reference)) match {
              case None => Left(BlockAppendError(s"References incorrect or non-existing block", block))
              case Some((referencedForgedBlock, referencedLiquidDiff, discarded)) =>
                if (referencedForgedBlock.signaturesValid().isRight) {
                  if (discarded.nonEmpty) {
                    microBlockForkStats.increment()
                    microBlockForkHeightStats.record(discarded.size)
                  }
                  historyWriter.appendBlock(referencedForgedBlock, ng.acceptedFeatures)(BlockDiffer.fromBlock(settings.blockchainSettings.functionalitySettings, historyReader,
                    composite(referencedLiquidDiff, currentPersistedBlocksState()),
                    Some(referencedForgedBlock), block))
                    .map { hardenedDiff =>
                      TxsInBlockchainStats.record(ng.transactions.size)
                      inMemDiffs.transform { imd =>
                        val withPrepended = prependCompactBlockDiff(referencedLiquidDiff, imd, maxTransactionsPerChunk)
                        val (inMem, toPersist: immutable.Seq[BlockDiff]) = splitAfterThreshold(withPrepended, minBlocksInMemory)(_.heightDiff)
                        toPersist.reverse.foreach(persisted.applyBlockDiff)
                        inMem
                      }
                      log.info(heights("After block application"))
                      Some((hardenedDiff, discarded.flatMap(_.transactionData)))
                    }
                } else {
                  val errorText = s"Forged block has invalid signature: base: ${ng.base}, requested reference: ${block.reference}"
                  log.error(errorText)
                  Left(BlockAppendError(errorText, block))
                }
            }
      }).map {
        _ map { case ((newBlockDiff, discacrded)) =>
          val height = historyWriter.height() + 1
          ngState.set(Some(new NgState(block, newBlockDiff, featuresApprovedWithBlock(block))))
          historyReader.lastBlockId().foreach(lastBlockId.onNext)
          log.info(s"$block appended. New height: $height)")
          discacrded
        }
      })
  }

  override def removeAfter(blockId: ByteStr): Either[ValidationError, Seq[Block]] = write { implicit l =>
    val ng = ngState()
    if (ng.exists(_.contains(blockId))) {
      log.trace("Resetting liquid block, no rollback is necessary")
      Right(Seq.empty)
    } else {
      historyWriter.heightOf(blockId) match {
        case None =>
          log.warn(s"removeAfter nonexistent block $blockId")
          Left(GenericError(s"Failed to rollback to nonexistent block $blockId"))
        case Some(requestedHeight) =>
          val discardedNgBlock = ng.map(_.bestLiquidBlock)
          ngState.set(None)

          val baseRolledBack = requestedHeight < historyWriter.height()
          val discardedHistoryBlocks = if (baseRolledBack) {
            log.info(s"Rollback to h=$requestedHeight started")
            val discarded = {
              var buf = Seq.empty[Block]
              while (historyWriter.height > requestedHeight)
                buf = historyWriter.discardBlock().toSeq ++ buf
              buf
            }
            if (requestedHeight < persisted.height) {
              log.info(s"Rollback to h=$requestedHeight requested. Persisted height=${persisted.height}, will drop state and reapply blockchain now")
              persisted.clear()
              syncPersistedAndInMemory()
            } else if (requestedHeight == persisted.height) {
              inMemDiffs.set(NEL.one(BlockDiff.empty))
            } else {
              val difference = persisted.height - requestedHeight
              inMemDiffs.transform { imd =>
                val remained = dropLeftIf(imd.toList)(_.map(bd => bd.heightDiff).sum > difference)
                unsafeDiffByRange(composite(remained, persisted), requestedHeight + 1) ++ remained
              }
            }
            log.info(s"Rollback to h=$requestedHeight completed:")
            discarded
          } else {
            log.debug(s"No rollback in history is necessary")
            Seq.empty[Block]
          }

          val totalDiscardedBlocks: Seq[Block] = discardedHistoryBlocks ++ discardedNgBlock.toSeq
          if (totalDiscardedBlocks.nonEmpty) lastBlockId.onNext(blockId)
          TxsInBlockchainStats.record(-totalDiscardedBlocks.size)
          Right(totalDiscardedBlocks)
      }
    }
  }

  override def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit] = write { implicit l =>
    ngState.mutate {
      case None =>
        Left(MicroBlockAppendError("No base block exists", microBlock))
      case Some(ng) if ng.base.signerData.generator.toAddress != microBlock.generator.toAddress =>
        Left(MicroBlockAppendError("Base block has been generated by another account", microBlock))
      case Some(ng) =>
        ng.lastMicroBlock match {
          case None if ng.base.uniqueId != microBlock.prevResBlockSig =>
            blockMicroForkStats.increment()
            Left(MicroBlockAppendError("It's first micro and it doesn't reference base block(which exists)", microBlock))
          case Some(prevMicro) if prevMicro.totalResBlockSig != microBlock.prevResBlockSig =>
            microMicroForkStats.increment()
            Left(MicroBlockAppendError("It doesn't reference last known microBlock(which exists)", microBlock))
          case _ =>
            for {
              _ <- microBlock.signaturesValid()
              diff <- BlockDiffer.fromMicroBlock(settings.blockchainSettings.functionalitySettings, historyReader,
                composite(ng.bestLiquidDiff.copy(snapshots = Map.empty), currentPersistedBlocksState()),
                historyWriter.lastBlock.map(_.timestamp), microBlock, ng.base.timestamp)
            } yield {
              log.info(s"$microBlock appended")
              ng.append(microBlock, diff, System.currentTimeMillis())
              lastBlockId.onNext(microBlock.totalResBlockSig)
            }
        }
    }
  }

  override def debugInfo(): StateDebugInfo = read { implicit l =>
    StateDebugInfo(
      persisted = HashInfo(height = persisted.height, hash = persisted.accountPortfoliosHash),
      inMemory = inMemDiffs().toList.map(d => HashInfo(height = d.heightDiff, hash = Hash.accountPortfolios(d.txsDiff.portfolios))),
      microBaseHash = ngState().map(ng => Hash.accountPortfolios(ng.baseBlockDiff.txsDiff.portfolios))
    )
  }

  override def persistedAccountPortfoliosHash(): Int = Hash.accountPortfolios(currentPersistedBlocksState().accountPortfolios)
}

object BlockchainUpdaterImpl extends ScorexLogging {

  private val blockMicroForkStats = Kamon.metrics.counter("block-micro-fork")
  private val microMicroForkStats = Kamon.metrics.counter("micro-micro-fork")
  private val microBlockForkStats = Kamon.metrics.counter("micro-block-fork")
  private val microBlockForkHeightStats = Kamon.metrics.histogram("micro-block-fork-height")
  private val forgeBlockTimeStats = Kamon.metrics.histogram("forge-block-time", Time.Milliseconds)

  def apply(persistedState: StateWriter with SnapshotStateReader,
            history: HistoryWriterImpl,
            settings: WavesSettings,
            synchronizationToken: ReentrantReadWriteLock): BlockchainUpdaterImpl = {
    val blockchainUpdater = new BlockchainUpdaterImpl(persistedState, settings, history, history, synchronizationToken)
    log.info(blockchainUpdater.heights("Constructing BlockchainUpdaterImpl"))
    blockchainUpdater.syncPersistedAndInMemory()
    blockchainUpdater
  }

  def areVersionsOfSameBlock(b1: Block, b2: Block): Boolean =
    b1.signerData.generator == b2.signerData.generator &&
      b1.consensusData.baseTarget == b2.consensusData.baseTarget &&
      b1.reference == b2.reference &&
      b1.timestamp == b2.timestamp
}
