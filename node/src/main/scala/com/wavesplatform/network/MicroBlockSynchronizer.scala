package com.wavesplatform.network

import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.MicroBlock
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.metrics.BlockStats
import com.wavesplatform.settings.SynchronizationSettings.MicroblockSynchronizerSettings
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.*
import monix.eval.{Coeval, Task}
import monix.execution.CancelableFuture
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable

import java.util.concurrent.TimeUnit
import scala.collection.mutable.Set as MSet
import scala.concurrent.duration.FiniteDuration

object MicroBlockSynchronizer extends ScorexLogging {

  def apply(
      settings: MicroblockSynchronizerSettings,
      isLightMode: Boolean,
      peerDatabase: PeerDatabase,
      lastBlockIdEvents: Observable[ByteStr],
      microblockInvs: ChannelObservable[MicroBlockInv],
      microblockResponses: ChannelObservable[MicroBlockResponse],
      microblockSnapshots: ChannelObservable[MicroBlockSnapshotResponse],
      scheduler: SchedulerService
  ): (Observable[MicroblockData.Remote], Coeval[CacheSizes]) = {

    implicit val schdlr: SchedulerService = scheduler

    val microBlockOwners     = cache[MicroBlockSignature, MSet[Channel]](settings.invCacheTimeout)
    val nextInvs             = cache[MicroBlockSignature, MicroBlockInv](settings.invCacheTimeout)
    val awaiting             = cache[MicroBlockSignature, MicroBlockInv](settings.invCacheTimeout)
    val waitingForSnapshot   = cache[MicroBlockSignature, MicroblockData.Remote](settings.invCacheTimeout)
    val successfullyReceived = cache[MicroBlockSignature, Object](settings.processedMicroBlocksCacheTimeout)
    val receivedSnapshots    = cache[MicroBlockSignature, Object](settings.processedMicroBlocksCacheTimeout)

    val lastBlockId = lastObserved(lastBlockIdEvents)

    def owners(totalRef: BlockId): Set[Channel] = Option(microBlockOwners.getIfPresent(totalRef)).getOrElse(MSet.empty).toSet

    def alreadyRequested(totalRef: MicroBlockSignature): Boolean = Option(awaiting.getIfPresent(totalRef)).isDefined

    val cacheSizesReporter = Coeval.eval {
      CacheSizes(microBlockOwners.size(), nextInvs.size(), awaiting.size(), successfullyReceived.size())
    }

    def requestData[A](
        totalBlockId: MicroBlockSignature,
        linkedData: A,
        awaitingCache: Cache[MicroBlockSignature, A],
        receivedDataCache: Cache[MicroBlockSignature, Object],
        requestMessageF: MicroBlockSignature => Message,
        dataType: String
    ): CancelableFuture[Unit] = {

      def randomOwner(exclude: Set[Channel]) = random(owners(totalBlockId) -- exclude)

      def task(attemptsAllowed: Int, exclude: Set[Channel]): Task[Unit] = Task.defer {
        if (attemptsAllowed <= 0) {
          log.trace(s"No more attempts left to download $dataType $totalBlockId")
          Task.unit
        } else if (Option(receivedDataCache.getIfPresent(totalBlockId)).isDefined) {
          Task.unit
        } else
          randomOwner(exclude) match {
            case None =>
              log.trace(s"No owners found for $dataType $totalBlockId")
              Task.unit
            case Some(channel) =>
              if (channel.isOpen) {
                log.trace(s"${id(channel)} Requesting $dataType $totalBlockId")
                val request = requestMessageF(totalBlockId)
                awaitingCache.put(totalBlockId, linkedData)
                channel.writeAndFlush(request)
                task(attemptsAllowed - 1, exclude + channel).delayExecution(settings.waitResponseTimeout)
              } else task(attemptsAllowed, exclude + channel)
          }
      }

      task(MicroBlockDownloadAttempts, Set.empty).runAsyncLogErr
    }

    def tryDownloadNext(prevBlockId: ByteStr): Unit = Option(nextInvs.getIfPresent(prevBlockId)).foreach { inv =>
      requestData(inv.totalBlockId, inv, awaiting, successfullyReceived, MicroBlockRequest.apply, "microblock")
    }

    lastBlockIdEvents
      .mapEval { f =>
        log.trace(s"Last block id is now $f")
        Task(tryDownloadNext(f))
      }
      .executeOn(scheduler)
      .logErr
      .subscribe()

    microblockInvs
      .mapEval { case (ch, mbInv @ MicroBlockInv(_, totalBlockId, reference, _)) =>
        Task.evalAsync {
          val sig =
            try mbInv.signaturesValid()
            catch {
              case t: Throwable =>
                log.error(s"Error validating signature", t)
                throw t
            }
          sig match {
            case Left(err) =>
              peerDatabase.blacklistAndClose(ch, err.toString)
            case Right(_) =>
              microBlockOwners.get(totalBlockId, () => MSet.empty) += ch
              nextInvs.get(
                reference,
                { () =>
                  BlockStats.inv(mbInv, ch)
                  mbInv
                }
              )
              lastBlockId() match {
                case Some(`reference`) if !alreadyRequested(totalBlockId) => tryDownloadNext(reference)
                case _ => // either the microblock has already been requested or it does no reference the last block
              }
          }
        }.logErr
      }
      .executeOn(scheduler)
      .logErr
      .subscribe()

    val mbResponsesObservable: Observable[MicroblockData.Remote] =
      microblockResponses.observeOn(scheduler).flatMap { case (ch, MicroBlockResponse(mb, totalRef)) =>
        successfullyReceived.put(totalRef, dummy)
        BlockStats.received(mb, ch, totalRef)
        Option(awaiting.getIfPresent(totalRef)) match {
          case None =>
            log.trace(s"${id(ch)} Received unexpected ${mb.stringRepr(totalRef)}")
            Observable.empty
          case Some(mi) =>
            log.trace(s"${id(ch)} Received ${mb.stringRepr(totalRef)}, as expected")
            awaiting.invalidate(totalRef)
            Observable(MicroblockData.Remote(mb, mi, ch, () => owners(totalRef), None))
        }
      }

    val observable: Observable[MicroblockData.Remote] = if (isLightMode) {
      mbResponsesObservable
        .mapEval(mbd =>
          Task.evalAsync {
            requestData(mbd.inv.totalBlockId, mbd, waitingForSnapshot, receivedSnapshots, MicroSnapshotRequest.apply, "microblock snapshot")
          }
        )
        .executeOn(scheduler)
        .logErr
        .subscribe()

      microblockSnapshots.observeOn(scheduler).flatMap { case (ch, snapshot) =>
        BlockStats.received(ch, snapshot.totalBlockId)
        Option(receivedSnapshots.getIfPresent(snapshot.totalBlockId)) match {
          case Some(_) =>
            waitingForSnapshot.invalidate(snapshot.totalBlockId)
            log.trace(s"${id(ch)} Received snapshot for processed microblock ${snapshot.totalBlockId}, ignoring")
            Observable.empty
          case None =>
            Option(waitingForSnapshot.getIfPresent(snapshot.totalBlockId)) match {
              case Some(mbd) =>
                receivedSnapshots.put(snapshot.totalBlockId, dummy)
                waitingForSnapshot.invalidate(snapshot.totalBlockId)
                log.trace(s"${id(ch)} Received microblock snapshot ${snapshot.totalBlockId}, as expected")
                Observable(mbd.copy(snapshot = Some(ch -> snapshot)))
              case None =>
                log.trace(s"${id(ch)} Received unexpected snapshot ${snapshot.totalBlockId}")
                Observable.empty
            }
        }
      }
    } else {
      mbResponsesObservable
    }

    (observable, cacheSizesReporter)
  }

  enum MicroblockData {
    def microBlock: MicroBlock
    def inv: MicroBlockInv
    case Local(microBlock: MicroBlock, inv: MicroBlockInv)
    case Remote(
        microBlock: MicroBlock,
        inv: MicroBlockInv,
        source: Channel,
        owners: () => Set[Channel],
        snapshot: Option[(snapshotSource: Channel, snapshotResponse: MicroBlockSnapshotResponse)]
    )
  }

  object MicroblockData {
    extension (m: MicroblockData) {
      def snapshot: Option[(snapshotSource: Channel, snapshotResponse: MicroBlockSnapshotResponse)] = m match {
        case r: MicroblockData.Remote => r.snapshot
        case _                        => None
      }
      
      def source: Option[Channel] = m match {
        case MicroblockData.Remote(source = source) => Some(source)
        case _ => None
      }
      
      def owners(): Set[Channel] = m match {
        case MicroblockData.Remote(owners = owners) => owners()
        case _ => Set.empty
      }
    }
  }

  type MicroBlockSignature = ByteStr

  private val MicroBlockDownloadAttempts = 2

  def random[T](s: Set[T]): Option[T] =
    if (s.isEmpty) None
    else {
      val n = util.Random.nextInt(s.size)
      s.drop(n).headOption
    }

  def cache[K <: AnyRef, V <: AnyRef](timeout: FiniteDuration): Cache[K, V] =
    CacheBuilder
      .newBuilder()
      .expireAfterWrite(timeout.toMillis, TimeUnit.MILLISECONDS)
      .build[K, V]()

  case class CacheSizes(microBlockOwners: Long, nextInvs: Long, awaiting: Long, successfullyReceived: Long)

  private val dummy = new Object()
}
