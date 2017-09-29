package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats._
import cats.implicits._
import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.BlockchainUpdaterImpl._
import com.wavesplatform.state2.NgState._
import com.wavesplatform.state2.diffs.BlockDiffer
import com.wavesplatform.state2.reader.CompositeStateReader.composite
import com.wavesplatform.state2.reader.StateReader
import kamon.Kamon
import kamon.metric.instrument.Time
import scorex.account.Address
import scorex.block.{Block, MicroBlock}
import scorex.transaction.ValidationError.{BlockAppendError, GenericError, MicroBlockAppendError}
import scorex.transaction._
import scorex.utils.ScorexLogging

class BlockchainUpdaterImpl private(persisted: StateWriter with StateReader,
                                    settings: FunctionalitySettings,
                                    featureProvider: FeatureProvider,
                                    minimumInMemoryDiffSize: Int,
                                    historyWriter: HistoryWriterImpl,
                                    val synchronizationToken: ReentrantReadWriteLock) extends BlockchainUpdater with BlockchainDebugInfo with ScorexLogging with Instrumented {

  private val topMemoryDiff = Synchronized(Monoid[BlockDiff].empty)
  private val bottomMemoryDiff = Synchronized(Monoid[BlockDiff].empty)
  private val ngState = Synchronized(Option.empty[NgState])

  private def unsafeDiffByRange(state: StateReader, from: Int, to: Int): BlockDiff = {
    val blocks = measureLog(s"Reading blocks from $from up to $to") {
      Range(from, to).map(historyWriter.blockBytes).par.map(b => Block.parseBytes(b.get).get).seq
    }
    measureLog(s"Building diff from $from up to $to") {
      BlockDiffer.unsafeDiffMany(settings, featureProvider, state, historyWriter.blockAt(from - 1))(blocks)
    }
  }

  private def logHeights(prefix: String): Unit = read { implicit l =>
    log.info(s"$prefix, total blocks: ${historyWriter.height()}, persisted: ${persisted.height}, " +
      s"topMemDiff: ${topMemoryDiff().heightDiff}, bottomMemDiff: ${bottomMemoryDiff().heightDiff}")
  }

  private def currentPersistedBlocksState: StateReader = read { implicit l =>
    composite(composite(persisted, () => bottomMemoryDiff()), () => topMemoryDiff())
  }

  def bestLiquidState: StateReader = read { implicit l => composite(currentPersistedBlocksState, () => ngState().map(_.bestLiquidDiff).orEmpty) }

  def historyReader: NgHistory with DebugNgHistory with FeatureProvider = read { implicit l => new NgHistoryReader(() => ngState(), historyWriter) }

  private def updatePersistedAndInMemory(): Unit = write { implicit l =>
    logHeights("State rebuild started")
    val persistFrom = persisted.height + 1
    val persistUpTo = historyWriter.height() - minimumInMemoryDiffSize + 1

    ranges(persistFrom, persistUpTo, minimumInMemoryDiffSize).foreach { case (head, last) =>
      val diffToBePersisted = unsafeDiffByRange(persisted, head, last)
      persisted.applyBlockDiff(diffToBePersisted)
    }

    bottomMemoryDiff.set(unsafeDiffByRange(persisted, persisted.height + 1, historyWriter.height() + 1))
    topMemoryDiff.set(BlockDiff.empty)
    logHeights("State rebuild finished")
  }

  override def processBlock(block: Block): Either[ValidationError, DiscardedTransactions] = write { implicit l =>
    if (topMemoryDiff().heightDiff >= minimumInMemoryDiffSize) {
      persisted.applyBlockDiff(bottomMemoryDiff())
      bottomMemoryDiff.set(topMemoryDiff())
      topMemoryDiff.set(BlockDiff.empty)
    }
    (ngState() match {
      case None =>
        historyWriter.lastBlock match {
          case Some(lastInner) if lastInner.uniqueId != block.reference =>
            val logDetails = s"The referenced block(${block.reference})" +
              s" ${if (historyWriter.contains(block.reference)) "exits, it's not last persisted" else "doesn't exist"}"
            Left(BlockAppendError(s"References incorrect or non-existing block: " + logDetails, block))
          case _ => BlockDiffer.fromBlock(settings, featureProvider, currentPersistedBlocksState, historyWriter.lastBlock, block).map((_, Seq.empty[Transaction]))
        }
      case Some(ng) if ng.base.reference == block.reference =>
        if (block.blockScore > ng.base.blockScore) {
          BlockDiffer.fromBlock(settings, featureProvider, currentPersistedBlocksState, historyWriter.lastBlock, block).map { diff =>
            log.trace(s"Better liquid block(score=${block.blockScore}) received and applied instead of existing(score=${ng.base.blockScore})")
            (diff, ng.transactions)
          }
        } else {
          Left(BlockAppendError(s"Competitor's liquid block(score=${block.blockScore}) is not better than existing(score=${ng.base.blockScore})", block))
        }
      case Some(ng) if !ng.contains(block.reference) =>
        Left(BlockAppendError(s"References incorrect or non-existing block", block))
      case Some(ng) =>
        val referencedLiquidDiff = ng.diffs(block.reference)._1
        val (referencedForgedBlock, discarded) = measureSuccessful(forgeBlockTimeStats, ng.forgeBlock(block.reference)).get
        if (referencedForgedBlock.signatureValid) {
          if (discarded.nonEmpty) {
            microBlockForkStats.increment()
            microBlockForkHeightStats.record(discarded.size)
          }
          historyWriter.appendBlock(referencedForgedBlock)(BlockDiffer.fromBlock(settings, featureProvider,
            composite(currentPersistedBlocksState, () => referencedLiquidDiff.copy(heightDiff = 1)),
            Some(referencedForgedBlock), block))
            .map { hardenedDiff =>
              topMemoryDiff.transform(Monoid.combine(_, referencedLiquidDiff))
              (hardenedDiff, discarded.flatMap(_.transactionData))
            }
        } else {
          val errorText = s"Forged block has invalid signature: base: ${ng.base}, micros: ${ng.micros}, requested reference: ${block.reference}"
          log.error(errorText)
          Left(BlockAppendError(errorText, block))
        }
    }).map { case ((newBlockDiff, discacrded)) =>
      ngState.set(Some(NgState(block, newBlockDiff, 0L)))
      val height = historyWriter.height() + 1
//      historyWriter.updateFeaturesState(Map(height -> block.supportedFeaturesIds))
      log.info(
        s"""Block ${block.uniqueId} -> ${trim(block.reference)} appended.
           | -- New height: $height, transactions: ${block.transactionData.size})""".stripMargin)
      discacrded
    }
  }

  override def removeAfter(blockId: ByteStr): Either[ValidationError, Seq[Transaction]] = write { implicit l =>
    val ng = ngState()
    if (ng.exists(_.contains(blockId))) {
      log.trace("No rollback necessary")
      Right(Seq.empty)
    } else {
      historyWriter.heightOf(blockId) match {
        case None =>
          log.warn(s"removeAfter nonexistent block $blockId")
          Left(GenericError(s"Failed to rollback to nonexistent block $blockId"))
        case Some(height) =>
          logHeights(s"Rollback to h=$height started")
          val discardedTransactions = Seq.newBuilder[Transaction]
          discardedTransactions ++= ng.toSeq.flatMap(_.transactions)
          ngState.set(None)

          while (historyWriter.height > height)
            discardedTransactions ++= historyWriter.discardBlock()
          if (height < persisted.height) {
            log.info(s"Rollback to h=$height requested. Persisted height=${persisted.height}, will drop state and reapply blockchain now")
            persisted.clear()
            updatePersistedAndInMemory()
          } else {
            if (bestLiquidState.height != height) {
              val persistedPlusBottomHeight = persisted.height + bottomMemoryDiff().heightDiff
              if (height > persistedPlusBottomHeight) {
                val newTopDiff = unsafeDiffByRange(composite(persisted, () => bottomMemoryDiff()), persistedPlusBottomHeight + 1, height + 1)
                topMemoryDiff.set(newTopDiff)
              } else {
                topMemoryDiff.set(BlockDiff.empty)
                if (height < persistedPlusBottomHeight)
                  bottomMemoryDiff.set(unsafeDiffByRange(persisted, persisted.height + 1, height + 1))
              }
            }
          }
          logHeights(s"Rollback to h=$height completed:")
          Right(discardedTransactions.result())
      }
    }
  }

  override def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit] = write { implicit l =>
    ngState() match {
      case None =>
        Left(MicroBlockAppendError("No base block exists", microBlock))
      case Some(ng) if ng.base.signerData.generator.toAddress != microBlock.generator.toAddress =>
        Left(MicroBlockAppendError("Base block has been generated by another account", microBlock))
      case Some(ng) =>
        ng.micros.headOption match {
          case None if ng.base.uniqueId != microBlock.prevResBlockSig =>
            blockMicroForkStats.increment()
            Left(MicroBlockAppendError("It's first micro and it doesn't reference base block(which exists)", microBlock))
          case Some(prevMicro) if prevMicro.totalResBlockSig != microBlock.prevResBlockSig =>
            microMicroForkStats.increment()
            Left(MicroBlockAppendError("It doesn't reference last known microBlock(which exists)", microBlock))
          case _ =>
            for {
              _ <- Signed.validateSignatures(microBlock)
              diff <- BlockDiffer.fromMicroBlock(settings, composite(currentPersistedBlocksState,
                () => ng.bestLiquidDiff.copy(snapshots = Map.empty)),
                historyWriter.lastBlock.map(_.timestamp), microBlock, ng.base.timestamp)
            } yield {
              log.info(s"MicroBlock ${trim(microBlock.totalResBlockSig)}~>${trim(microBlock.prevResBlockSig)} appended. " +
                s"-- with ${microBlock.transactionData.size} transactions")
              ngState.set(Some(ng + (microBlock, Monoid.combine(ng.bestLiquidDiff, diff), System.currentTimeMillis())))
            }
        }
    }
  }

  override def debugInfo(): StateDebugInfo = read {
    implicit l =>
      StateDebugInfo(persisted = HashInfo(height = persisted.height, hash = persisted.accountPortfoliosHash),
        top = HashInfo(height = topMemoryDiff().heightDiff, hash = Hash.accountPortfolios(topMemoryDiff().txsDiff.portfolios)),
        bottom = HashInfo(height = bottomMemoryDiff().heightDiff, hash = Hash.accountPortfolios(bottomMemoryDiff().txsDiff.portfolios)),
        microBaseHash = ngState().map(ng => Hash.accountPortfolios(ng.diffs(ng.base.uniqueId)._1.txsDiff.portfolios))
      )
  }

  override def persistedAccountPortfoliosHash(): Int = Hash.accountPortfolios(currentPersistedBlocksState.accountPortfolios)

  override def topDiff(): Map[Address, Portfolio] = read {
    implicit l => topMemoryDiff().txsDiff.portfolios
  }

  override def bottomDiff(): Map[Address, Portfolio] = read {
    implicit l => bottomMemoryDiff().txsDiff.portfolios
  }
}

object BlockchainUpdaterImpl {

  private val blockMicroForkStats = Kamon.metrics.counter("block-micro-fork")
  private val microMicroForkStats = Kamon.metrics.counter("micro-micro-fork")
  private val microBlockForkStats = Kamon.metrics.counter("micro-block-fork")
  private val microBlockForkHeightStats = Kamon.metrics.histogram("micro-block-fork-height")
  private val forgeBlockTimeStats = Kamon.metrics.histogram("forge-block-time", Time.Milliseconds)

  def apply(persistedState: StateWriter with StateReader,
            history: HistoryWriterImpl,
            functionalitySettings: FunctionalitySettings,
            minimumInMemoryDiffSize: Int,
            synchronizationToken: ReentrantReadWriteLock): BlockchainUpdaterImpl = {
    val blockchainUpdater =
      new BlockchainUpdaterImpl(persistedState, functionalitySettings, history, minimumInMemoryDiffSize, history, synchronizationToken)
    blockchainUpdater.logHeights("Constructing BlockchainUpdaterImpl")
    blockchainUpdater.updatePersistedAndInMemory()
    blockchainUpdater
  }

  def ranges(from: Int, to: Int, by: Int): Stream[(Int, Int)] =
    if (from + by < to)
      (from, from + by) #:: ranges(from + by, to, by)
    else
      (from, to) #:: Stream.empty[(Int, Int)]

}
