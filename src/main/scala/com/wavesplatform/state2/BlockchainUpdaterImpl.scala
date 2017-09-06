package com.wavesplatform.state2

import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.metrics.{Instrumented, TxsInBlockchainStats}
import com.wavesplatform.mining.MiningEstimators
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.diffs.BlockDiffer
import com.wavesplatform.state2.reader.CompositeStateReader.composite
import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.utils.{UnsupportedFeature, forceStopApplication}
import kamon.Kamon
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import scorex.block.{Block, MicroBlock}
import scorex.transaction.ValidationError.{BlockAppendError, GenericError, MicroBlockAppendError}
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

class BlockchainUpdaterImpl(persisted: StateWriter with SnapshotStateReader,
                                    settings: WavesSettings,
                                    time: Time,
                                    history: History) extends BlockchainUpdater with ScorexLogging with Instrumented {

  import com.wavesplatform.state2.BlockchainUpdaterImpl._
  import settings.blockchainSettings.functionalitySettings

  private lazy val maxBlockReadinessAge = settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed.toMillis

  private var ngState = Option.empty[NgState]

  private val internalLastBlockInfo = ConcurrentSubject.publish[LastBlockInfo](monix.execution.Scheduler.singleThread("last-block-info-publisher"))
  override val lastBlockInfo: Observable[LastBlockInfo] = internalLastBlockInfo.cache(1)
  lastBlockInfo.subscribe()(monix.execution.Scheduler.global) // Start caching

  def blockchainReady: Boolean = {
    val lastBlock = historyReader.lastBlockTimestamp.get
    lastBlock + maxBlockReadinessAge > time.correctedTime()
  }

  def historyReader: NgHistory with DebugNgHistory with FeatureProvider = new NgHistoryReader(() => ngState, history, functionalitySettings)
  def stateReader: SnapshotStateReader = composite(persisted, ngState.map(_.bestLiquidDiff))

  // Store last block information in a cache
  historyReader.lastBlockId.foreach { id =>
    internalLastBlockInfo.onNext(LastBlockInfo(id, historyReader.height, historyReader.score, blockchainReady))
  }

  private def displayFeatures(s: Set[Short]): String = s"FEATURE${if (s.size > 1) "S"} ${s.mkString(", ")}${if (s.size > 1) "HAVE BEEN" else "HAS BEEN"}"

  private def featuresApprovedWithBlock(block: Block): Set[Short] = {
    val height = history.height + 1

    val featuresCheckPeriod = functionalitySettings.activationWindowSize(height)
    val blocksForFeatureActivation = functionalitySettings.blocksForFeatureActivation(height)

    if (height % featuresCheckPeriod == 0) {
      val approvedFeatures = history.featureVotes(height)
        .map { case (feature, votes) => feature -> (if (block.featureVotes.contains(feature)) votes + 1 else votes) }
        .filter { case (_, votes) => votes >= blocksForFeatureActivation }
        .keySet

      if (approvedFeatures.nonEmpty) log.info(s"${displayFeatures(approvedFeatures)} APPROVED ON BLOCKCHAIN")

      val unimplementedApproved = approvedFeatures.diff(BlockchainFeatures.implemented)
      if (unimplementedApproved.nonEmpty) {
        log.warn(s"UNIMPLEMENTED ${displayFeatures(unimplementedApproved)} APPROVED ON BLOCKCHAIN")
        log.warn("PLEASE, UPDATE THE NODE AS SOON AS POSSIBLE")
        log.warn("OTHERWISE THE NODE WILL BE STOPPED OR FORKED UPON FEATURE ACTIVATION")
      }

      val activatedFeatures = history.activatedFeatures(height)

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
    else {

      Set.empty
    }
  }

  override def processBlock(block: Block): Either[ValidationError, Option[DiscardedTransactions]] = {
    val height = history.height
    val notImplementedFeatures = history.activatedFeatures(height).diff(BlockchainFeatures.implemented)

    Either.cond(!settings.featuresSettings.autoShutdownOnUnsupportedFeature || notImplementedFeatures.isEmpty, (),
      GenericError(s"UNIMPLEMENTED ${displayFeatures(notImplementedFeatures)} ACTIVATED ON BLOCKCHAIN, UPDATE THE NODE IMMEDIATELY")).flatMap(_ =>
      (ngState match {
        case None =>
          history.lastBlockId match {
            case Some(uniqueId) if uniqueId != block.reference =>
              val logDetails = s"The referenced block(${block.reference})" +
                s" ${if (history.contains(block.reference)) "exits, it's not last persisted" else "doesn't exist"}"
              Left(BlockAppendError(s"References incorrect or non-existing block: " + logDetails, block))
            case _ =>
              BlockDiffer
                .fromBlock(functionalitySettings, history, persisted, history.lastBlock, block)
                .map(d => Some((d, Seq.empty[Transaction])))
          }
        case Some(ng) =>
          if (ng.base.reference == block.reference) {
            if (block.blockScore() > ng.base.blockScore()) {
              BlockDiffer.fromBlock(functionalitySettings, history, persisted, history.lastBlock, block).map { diff =>
                log.trace(s"Better liquid block(score=${block.blockScore()}) received and applied instead of existing(score=${ng.base.blockScore()})")
                Some((diff, ng.transactions))
              }
            } else if (areVersionsOfSameBlock(block, ng.base)) {
              if (block.transactionData.lengthCompare(ng.transactions.size) <= 0) {
                log.trace(s"Existing liquid block is better than new one, discarding $block")
                Right(None)
              } else {
                log.trace(s"New liquid block is better version of exsting, swapping")
                BlockDiffer.fromBlock(functionalitySettings, history, persisted, history.lastBlock, block).map(d => Some((d, Seq.empty[Transaction])))
              }
            } else Left(BlockAppendError(s"Competitors liquid block $block(score=${block.blockScore()}) is not better than existing (ng.base ${ng.base}(score=${ng.base.blockScore()}))", block))
          } else
            measureSuccessful(forgeBlockTimeStats, ng.totalDiffOf(block.reference)) match {
              case None => Left(BlockAppendError(s"References incorrect or non-existing block", block))
              case Some((referencedForgedBlock, referencedLiquidDiff, discarded)) =>
                if (referencedForgedBlock.signaturesValid().isRight) {
                  if (discarded.nonEmpty) {
                    microBlockForkStats.increment()
                    microBlockForkHeightStats.record(discarded.size)
                  }

                  val diff = BlockDiffer.fromBlock(
                    functionalitySettings,
                    historyReader,
                    composite(persisted, referencedLiquidDiff),
                    Some(referencedForgedBlock),
                    block)

                  diff.map { hardenedDiff =>
                    persisted.append(referencedLiquidDiff, referencedForgedBlock)
                    TxsInBlockchainStats.record(ng.transactions.size)
                    Some((hardenedDiff, discarded.flatMap(_.transactionData)))
                  }
                } else {
                  val errorText = s"Forged block has invalid signature: base: ${ng.base}, requested reference: ${block.reference}"
                  log.error(errorText)
                  Left(BlockAppendError(errorText, block))
                }
            }
      }).map {
        _ map { case ((newBlockDiff, discarded)) =>
          val height = history.height + 1
          val estimators = MiningEstimators(settings.minerSettings, history, history.height)
          ngState = Some(new NgState(block, newBlockDiff, featuresApprovedWithBlock(block), estimators.total))
          historyReader.lastBlockId.foreach(id =>
            internalLastBlockInfo.onNext(LastBlockInfo(id, historyReader.height, historyReader.score, blockchainReady)))
          if (height % 100 == 0) {
            log.info(s"New height: $height")
          }
          discarded
        }
      })
  }

  override def removeAfter(blockId: ByteStr): Either[ValidationError, Seq[Block]] = {
    val ng = ngState
    if (ng.exists(_.contains(blockId))) {
      log.trace("Resetting liquid block, no rollback is necessary")
      Right(Seq.empty)
    } else {
      val discardedNgBlock = ng.map(_.bestLiquidBlock).toSeq
      ngState = None
      Right(persisted.rollbackTo(blockId) ++ discardedNgBlock)
    }
  }

  override def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit] = {
    ngState match {
      case None =>
        Left(MicroBlockAppendError("No base block exists", microBlock))
      case Some(ng) if ng.base.signerData.generator.toAddress != microBlock.sender.toAddress =>
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
              diff <- BlockDiffer.fromMicroBlock(functionalitySettings, historyReader,
                composite(persisted, ng.bestLiquidDiff),
                history.lastBlockTimestamp, microBlock, ng.base.timestamp)
              _ <- Either.cond(ng.append(microBlock, diff, System.currentTimeMillis), (), MicroBlockAppendError("Limit of txs was reached", microBlock))
            } yield {
              log.info(s"$microBlock appended")
              internalLastBlockInfo.onNext(LastBlockInfo(microBlock.totalResBlockSig, historyReader.height, historyReader.score, ready = true))
            }
        }
    }
  }

  def shutdown(): Unit = {
    internalLastBlockInfo.onComplete()
  }
}

object BlockchainUpdaterImpl extends ScorexLogging {

  import kamon.metric.instrument.{Time => KTime}

  private val blockMicroForkStats = Kamon.metrics.counter("block-micro-fork")
  private val microMicroForkStats = Kamon.metrics.counter("micro-micro-fork")
  private val microBlockForkStats = Kamon.metrics.counter("micro-block-fork")
  private val microBlockForkHeightStats = Kamon.metrics.histogram("micro-block-fork-height")
  private val forgeBlockTimeStats = Kamon.metrics.histogram("forge-block-time", KTime.Milliseconds)

  def areVersionsOfSameBlock(b1: Block, b2: Block): Boolean =
    b1.signerData.generator == b2.signerData.generator &&
      b1.consensusData.baseTarget == b2.consensusData.baseTarget &&
      b1.reference == b2.reference &&
      b1.timestamp == b2.timestamp
}
