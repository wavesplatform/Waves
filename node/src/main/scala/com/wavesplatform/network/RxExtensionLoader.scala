package com.wavesplatform.network

import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.BlockStats
import com.wavesplatform.network.RxExtensionLoader.ApplierState.Buffer
import com.wavesplatform.network.RxExtensionLoader.LoaderState.WithPeer
import com.wavesplatform.network.RxScoreObserver.{ChannelClosedAndSyncWith, SyncWith}
import com.wavesplatform.state.ParSignatureChecker
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.*
import monix.eval.{Coeval, Task}
import monix.execution.CancelableFuture
import monix.execution.schedulers.SchedulerService
import monix.reactive.subjects.{ConcurrentSubject, Subject}
import monix.reactive.{Observable, Observer}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

case class ExtensionBlocks(remoteScore: BigInt, blocks: Seq[Block], snapshots: Map[BlockId, BlockSnapshot]) {
  override def toString: String = s"ExtensionBlocks($remoteScore, ${formatSignatures(blocks.map(_.id()))}"
}

object RxExtensionLoader extends ScorexLogging {

  type ApplyExtensionResult = Either[ValidationError, Option[BigInt]]
  private val dummy = new Object()

  def apply(
      syncTimeOut: FiniteDuration,
      processedBlocksCacheTimeout: FiniteDuration,
      isLightMode: Boolean,
      lastBlockIds: Coeval[Seq[ByteStr]],
      peerDatabase: PeerDatabase,
      invalidBlocks: InvalidBlockStorage,
      blocks: Observable[(Channel, Block)],
      signatures: Observable[(Channel, Signatures)],
      snapshots: Observable[(Channel, BlockSnapshot)],
      syncWithChannelClosed: Observable[ChannelClosedAndSyncWith],
      scheduler: SchedulerService,
      timeoutSubject: Subject[Channel, Channel]
  )(
      extensionApplier: (Channel, ExtensionBlocks) => Task[ApplyExtensionResult]
  ): (Observable[(Channel, Block, Option[BlockSnapshot])], Coeval[State], RxExtensionLoaderShutdownHook) = {

    implicit val schdlr: SchedulerService = scheduler

    val extensions: ConcurrentSubject[(Channel, ExtensionBlocks), (Channel, ExtensionBlocks)] = ConcurrentSubject.publish[(Channel, ExtensionBlocks)]
    val simpleBlocksWithSnapshot: ConcurrentSubject[(Channel, Block, Option[BlockSnapshot]), (Channel, Block, Option[BlockSnapshot])] =
      ConcurrentSubject.publish[(Channel, Block, Option[BlockSnapshot])]
    @volatile var stateValue: State            = State(LoaderState.Idle, ApplierState.Idle)
    val lastSyncWith: Coeval[Option[SyncWith]] = lastObserved(syncWithChannelClosed.map(_.syncWith))

    val pendingBlocks     = cache[(Channel, BlockId), Block](syncTimeOut)
    val receivedSnapshots = cache[BlockId, Object](processedBlocksCacheTimeout)

    def scheduleBlacklist(ch: Channel, reason: String): Task[Unit] =
      Task {
        timeoutSubject.onNext(ch)
        peerDatabase.blacklistAndClose(ch, reason)
      }.delayExecution(syncTimeOut)

    def syncNext(state: State, syncWith: SyncWith = lastSyncWith().flatten): State =
      syncWith match {
        case None =>
          log.trace("Last bestChannel is None, state is up to date")
          state.withIdleLoader
        case Some(best) =>
          state.loaderState match {
            case wp: WithPeer =>
              log.trace(s"${id(wp.channel.channel)} Already syncing, no need to sync next, $state")
              state
            case LoaderState.Idle =>
              val maybeKnownSigs = state.applierState match {
                case ApplierState.Idle                => Some((lastBlockIds(), false))
                case ApplierState.Applying(None, ext) => Some((ext.blocks.map(_.id()).reverse, true))
                case _                                => None
              }
              maybeKnownSigs match {
                case Some((knownSigs, optimistic)) =>
                  val ch = best.channel
                  log.debug(
                    s"${id(ch)} Requesting signatures${if (optimistic) " optimistically" else ""}, last ${knownSigs.length} are ${formatSignatures(knownSigs)}"
                  )

                  val blacklisting = scheduleBlacklist(ch, s"Timeout loading extension").runAsyncLogErr
                  ch.writeAndFlush(GetSignatures(knownSigs)).addListener { f: ChannelFuture =>
                    if (!f.isSuccess) log.trace(s"Error requesting signatures: $ch", f.cause())
                  }

                  state.withLoaderState(LoaderState.ExpectingSignatures(best, knownSigs, blacklisting))
                case None =>
                  log.trace(s"Holding on requesting next sigs, $state")
                  state
              }
          }
      }

    def onNewSyncWithChannelClosed(state: State, cc: ChannelClosedAndSyncWith): State = {
      cc match {
        case ChannelClosedAndSyncWith(_, None) =>
          state.loaderState match {
            case _: LoaderState.WithPeer => state.withIdleLoader
            case _                       => state
          }
        case ChannelClosedAndSyncWith(None, Some(bestChannel)) =>
          log.trace(s"New SyncWith: $bestChannel, currentState = $state")
          syncNext(state, Some(bestChannel))
        case ChannelClosedAndSyncWith(Some(closedChannel), Some(bestChannel)) =>
          state.loaderState match {
            case wp: LoaderState.WithPeer if closedChannel != wp.channel.channel => state
            case _ =>
              log.trace(s"Switching to next best channel: state=$state, cc=$cc, bestChannel=$bestChannel")
              syncNext(state.withIdleLoader, Some(bestChannel))
          }
      }
    }

    def onNewSignatures(state: State, ch: Channel, sigs: Signatures): State = {
      state.loaderState match {
        case LoaderState.ExpectingSignatures(c, _, _) if c.channel == ch && sigs.signatures.isEmpty =>
          peerDatabase.blacklistAndClose(ch, s"Peer did not return any signatures and is likely on a fork")
          syncNext(state.withIdleLoader)
        case LoaderState.ExpectingSignatures(c, known, _) if c.channel == ch =>
          val (_, unknown) = sigs.signatures.span(id => known.contains(id))

          val firstInvalid = sigs.signatures.view.flatMap { sig =>
            invalidBlocks.find(sig).map(sig -> _)
          }.headOption

          firstInvalid match {
            case Some((invalidBlock, reason)) =>
              peerDatabase.blacklistAndClose(ch, s"Signatures contain invalid block(s): $invalidBlock, $reason")
              syncNext(state.withIdleLoader)
            case None =>
              if (unknown.isEmpty) {
                log.trace(s"${id(ch)} Received empty extension signatures list, sync with node complete")
                state.withIdleLoader
              } else {
                log.trace(s"${id(ch)} Requesting ${unknown.size} blocks")
                val blacklistingAsync = scheduleBlacklist(ch, "Timeout loading first requested block").runAsyncLogErr
                unknown.foreach { s =>
                  ch.write(GetBlock(s))
                  if (isLightMode) ch.write(GetSnapshot(s))
                }
                ch.flush()
                state.withLoaderState(
                  LoaderState.ExpectingBlocksWithSnapshots(
                    c,
                    unknown,
                    unknown.toSet,
                    Set.empty,
                    if (isLightMode) unknown.toSet else Set.empty,
                    Map.empty,
                    blacklistingAsync
                  )
                )
              }
          }
        case _ =>
          log.trace(s"${id(ch)} Received unexpected signatures ${formatSignatures(sigs.signatures)}, ignoring at $state")
          state
      }
    }

    def onBlock(state: State, ch: Channel, block: Block): State = {
      state.loaderState match {
        case LoaderState.ExpectingBlocksWithSnapshots(c, requested, expectedBlocks, receivedBlocks, expectedSnapshots, receivedSnapshots, _)
            if c.channel == ch && expectedBlocks.contains(block.id()) =>
          BlockStats.received(block, BlockStats.Source.Ext, ch)
          ParSignatureChecker.checkBlockSignature(block)
          if (expectedBlocks == Set(block.id()) && expectedSnapshots.isEmpty) {
            val blockById = (receivedBlocks + block).map(b => b.id() -> b).toMap
            val ext       = ExtensionBlocks(c.score, requested.map(blockById), receivedSnapshots)
            log.debug(s"${id(ch)} $ext successfully received")
            extensionLoadingFinished(state.withIdleLoader, ext, ch)
          } else if (expectedBlocks == Set(block.id())) {
            val blacklistAsync = scheduleBlacklist(
              ch,
              s"Timeout loading one of requested block snapshots, non-received: ${if (expectedSnapshots.size == 1) s"one=${requested.last.trim}"
              else s"total=${expectedSnapshots.size}"}"
            ).runAsyncLogErr
            state.withLoaderState(
              LoaderState.ExpectingBlocksWithSnapshots(
                c,
                requested,
                expectedBlocks - block.id(),
                receivedBlocks + block,
                expectedSnapshots,
                receivedSnapshots,
                blacklistAsync
              )
            )
          } else {
            val snapshotsInfo =
              if (isLightMode)
                s", non-received snapshots: ${if (expectedSnapshots.size == 1) s"one=${requested.last.trim}" else s"total=${expectedSnapshots.size}"}"
              else ""
            val blacklistAsync = scheduleBlacklist(
              ch,
              s"Timeout loading one of requested blocks or snapshots, non-received blocks: ${
                val totalLeft = expectedBlocks.size - 1
                if (totalLeft == 1) "one=" + requested.last.trim
                else "total=" + totalLeft.toString
              }$snapshotsInfo"
            ).runAsyncLogErr
            state.withLoaderState(
              LoaderState.ExpectingBlocksWithSnapshots(
                c,
                requested,
                expectedBlocks - block.id(),
                receivedBlocks + block,
                expectedSnapshots,
                receivedSnapshots,
                blacklistAsync
              )
            )
          }
        case _ =>
          BlockStats.received(block, BlockStats.Source.Broadcast, ch)
          if (!isLightMode || block.transactionData.isEmpty) {
            simpleBlocksWithSnapshot.onNext((ch, block, None))
          } else {
            val blockId = block.id()
            if (Option(receivedSnapshots.getIfPresent(blockId)).isEmpty) {
              pendingBlocks.put((ch, blockId), block)
              ch.writeAndFlush(GetSnapshot(blockId))
            }
          }
          state
      }
    }

    def onSnapshot(state: State, ch: Channel, snapshot: BlockSnapshot): State = {
      if (isLightMode) {
        state.loaderState match {
          case LoaderState.ExpectingBlocksWithSnapshots(c, requested, expectedBlocks, receivedBlocks, expectedSnapshots, receivedSnapshots, _)
              if c.channel == ch && expectedSnapshots.contains(snapshot.blockId) =>
            BlockStats.received(snapshot, BlockStats.Source.Ext, ch)
            if (expectedSnapshots == Set(snapshot.blockId) && expectedBlocks.isEmpty) {
              val blockById = receivedBlocks.map(b => b.id() -> b).toMap
              val ext       = ExtensionBlocks(c.score, requested.map(blockById), receivedSnapshots.updated(snapshot.blockId, snapshot))
              log.debug(s"${id(ch)} $ext successfully received")
              extensionLoadingFinished(state.withIdleLoader, ext, ch)
            } else if (expectedSnapshots == Set(snapshot.blockId)) {
              val blacklistAsync = scheduleBlacklist(
                ch,
                s"Timeout loading one of requested blocks, non-received: ${if (expectedBlocks.size == 1) s"one=${requested.last.trim}"
                else s"total=${expectedBlocks.size}"}"
              ).runAsyncLogErr
              state.withLoaderState(
                LoaderState.ExpectingBlocksWithSnapshots(
                  c,
                  requested,
                  expectedBlocks,
                  receivedBlocks,
                  expectedSnapshots - snapshot.blockId,
                  receivedSnapshots.updated(snapshot.blockId, snapshot),
                  blacklistAsync
                )
              )
            } else {
              val blacklistAsync = scheduleBlacklist(
                ch,
                s"Timeout loading one of requested blocks or snapshots, non-received blocks: ${if (expectedBlocks.size == 1) s"one=${requested.last.trim}"
                else s"total=${expectedBlocks.size}"}, non-received snapshots: ${
                  val totalLeft = expectedSnapshots.size - 1
                  if (totalLeft == 1) s"one=${requested.last.trim}" else s"total=$totalLeft"
                }"
              ).runAsyncLogErr
              state.withLoaderState(
                LoaderState.ExpectingBlocksWithSnapshots(
                  c,
                  requested,
                  expectedBlocks,
                  receivedBlocks,
                  expectedSnapshots - snapshot.blockId,
                  receivedSnapshots.updated(snapshot.blockId, snapshot),
                  blacklistAsync
                )
              )
            }
          case _ =>
            BlockStats.received(snapshot, BlockStats.Source.Broadcast, ch)
            Option(receivedSnapshots.getIfPresent(snapshot.blockId)) match {
              case Some(_) =>
                pendingBlocks.invalidate(snapshot.blockId)
                log.trace(s"${id(ch)} Received snapshot for processed block ${snapshot.blockId}, ignoring at $state")
              case _ =>
                Option(pendingBlocks.getIfPresent((ch, snapshot.blockId))) match {
                  case Some(block) =>
                    simpleBlocksWithSnapshot.onNext((ch, block, Some(snapshot)))
                    receivedSnapshots.put(snapshot.blockId, dummy)
                    pendingBlocks.invalidate(snapshot.blockId)
                  case None =>
                    log.trace(s"${id(ch)} Received unexpected snapshot ${snapshot.blockId}, ignoring at $state")
                }
            }

            state
        }
      } else {
        log.trace(s"${id(ch)} Received unexpected snapshot ${snapshot.blockId}, ignoring at $state")
        state
      }
    }

    def extensionLoadingFinished(state: State, extension: ExtensionBlocks, ch: Channel): State = {
      state.applierState match {
        case ApplierState.Idle =>
          extensions.onNext(ch -> extension)
          syncNext(state.copy(applierState = ApplierState.Applying(None, extension)))
        case s @ ApplierState.Applying(None, applying) =>
          log.trace(s"An optimistic extension was received: $extension, but applying $applying now")
          state.copy(applierState = s.copy(buf = Some(Buffer(ch, extension))))
        case _ =>
          log.warn(s"Overflow, discarding $extension")
          state
      }
    }

    def onExtensionApplied(state: State, extension: ExtensionBlocks, applicationResult: ApplyExtensionResult): State = {
      log.trace(s"Applying $extension finished with $applicationResult")
      state.applierState match {
        case ApplierState.Idle =>
          log.warn(s"Applied $extension but ApplierState is Idle")
          state
        case ApplierState.Applying(maybeBuffer, applying) =>
          if (applying != extension) log.warn(s"Applied $extension doesn't match expected $applying")
          maybeBuffer match {
            case None => state.copy(applierState = ApplierState.Idle)
            case Some(Buffer(nextChannel, nextExtension)) =>
              applicationResult match {
                case Left(_) =>
                  log.debug(s"Failed to apply $extension, discarding cached as well")
                  syncNext(state.copy(applierState = ApplierState.Idle))
                case Right(_) =>
                  log.trace(s"Successfully applied $extension, starting to apply an optimistically loaded one: $nextExtension")
                  extensions.onNext(nextChannel -> nextExtension)
                  syncNext(state.copy(applierState = ApplierState.Applying(None, nextExtension)))
              }
          }
      }
    }

    def appliedExtensions: Observable[(Channel, ExtensionBlocks, ApplyExtensionResult)] = {
      def apply(x: (Channel, ExtensionBlocks)): Task[ApplyExtensionResult] = Function.tupled(extensionApplier)(x)

      extensions.mapEval { x =>
        apply(x)
          .asyncBoundary(scheduler)
          .onErrorHandle { err =>
            log.error("Error during extension applying", err)
            Left(GenericError(err))
          }
          .map((x._1, x._2, _))
      }
    }

    Observable(
      signatures.observeOn(scheduler).map { case (ch, sigs) => stateValue = onNewSignatures(stateValue, ch, sigs) },
      blocks.observeOn(scheduler).map { case (ch, block) => stateValue = onBlock(stateValue, ch, block) },
      snapshots.observeOn(scheduler).map { case (ch, snapshot) => stateValue = onSnapshot(stateValue, ch, snapshot) },
      syncWithChannelClosed.observeOn(scheduler).map { ch =>
        stateValue = onNewSyncWithChannelClosed(stateValue, ch)
      },
      appliedExtensions.map { case (_, extensionBlocks, ar) => stateValue = onExtensionApplied(stateValue, extensionBlocks, ar) }
    ).merge
      .map { _ =>
        log.trace(s"Current state: $stateValue")
      }
      .logErr
      .subscribe()

    (simpleBlocksWithSnapshot, Coeval.eval(stateValue), RxExtensionLoaderShutdownHook(extensions, simpleBlocksWithSnapshot))
  }

  private def cache[K <: AnyRef, V <: AnyRef](timeout: FiniteDuration): Cache[K, V] =
    CacheBuilder
      .newBuilder()
      .expireAfterWrite(timeout.toMillis, TimeUnit.MILLISECONDS)
      .build[K, V]()

  sealed trait LoaderState

  object LoaderState {

    sealed trait WithPeer extends LoaderState {
      def channel: BestChannel
      def timeout: CancelableFuture[Unit]
    }

    case object Idle extends LoaderState

    case class ExpectingSignatures(channel: BestChannel, known: Seq[BlockId], timeout: CancelableFuture[Unit]) extends WithPeer {
      override def toString: String = s"ExpectingSignatures($channel)"
    }

    case class ExpectingBlocksWithSnapshots(
        channel: BestChannel,
        allBlocks: Seq[BlockId],
        expectedBlocks: Set[BlockId],
        receivedBlocks: Set[Block],
        expectedSnapshots: Set[BlockId],
        receivedSnapshots: Map[BlockId, BlockSnapshot],
        timeout: CancelableFuture[Unit]
    ) extends WithPeer {
      override def toString: String =
        s"ExpectingBlocks($channel,totalBlocks=${allBlocks.size},received=${receivedBlocks.size},expected=${if (expectedBlocks.size == 1) expectedBlocks.head.trim
        else expectedBlocks.size})"
    }

  }

  case class RxExtensionLoaderShutdownHook(
      extensionChannel: Observer[(Channel, ExtensionBlocks)],
      simpleBlocksWithSnapshotChannel: Observer[(Channel, Block, Option[BlockSnapshot])]
  ) {
    def shutdown(): Unit = {
      extensionChannel.onComplete()
      simpleBlocksWithSnapshotChannel.onComplete()
    }
  }

  case class State(loaderState: LoaderState, applierState: ApplierState) {
    def withLoaderState(newLoaderState: LoaderState): State = {
      loaderState match {
        case wp: WithPeer => wp.timeout.cancel()
        case _            =>
      }
      State(newLoaderState, applierState)
    }

    def withIdleLoader: State = withLoaderState(LoaderState.Idle)
  }

  sealed trait ApplierState

  object ApplierState {

    case object Idle extends ApplierState

    case class Buffer(ch: Channel, ext: ExtensionBlocks) {
      override def toString: String = s"Buffer($ext from ${id(ch)})"
    }

    case class Applying(buf: Option[Buffer], applying: ExtensionBlocks) extends ApplierState

  }

}
