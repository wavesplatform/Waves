package com.wavesplatform.network

import java.util.concurrent.TimeUnit

import com.google.common.cache.{Cache, CacheBuilder, RemovalNotification}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.MicroBlock
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.metrics.BlockStats
import com.wavesplatform.settings.SynchronizationSettings.MicroblockSynchronizerSettings
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel._
import monix.eval.{Coeval, Task}
import monix.execution.CancelableFuture
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable

import scala.collection.mutable.{Set => MSet}
import scala.concurrent.duration.FiniteDuration

object MicroBlockSynchronizer extends ScorexLogging {

  def apply(
      settings: MicroblockSynchronizerSettings,
      peerDatabase: PeerDatabase,
      lastBlockIdEvents: Observable[ByteStr],
      microblockInvs: ChannelObservable[MicroBlockInv],
      microblockResponses: ChannelObservable[MicroBlockResponse],
      scheduler: SchedulerService
  ): (Observable[(Channel, MicroblockData)], Coeval[CacheSizes]) = {

    implicit val schdlr: SchedulerService = scheduler

    val microBlockOwners     = cache[MicroBlockSignature, MSet[Channel]]("microBlockOwners", settings.invCacheTimeout)
    val nextInvs             = cache[MicroBlockSignature, MicroBlockInv]("nextInvs", settings.invCacheTimeout)
    val awaiting             = cache[MicroBlockSignature, MicroBlockInv]("awaiting", settings.invCacheTimeout)
    val successfullyReceived = cache[MicroBlockSignature, Object]("successfullyReceived", settings.processedMicroBlocksCacheTimeout)

    val lastBlockId = lastObserved(lastBlockIdEvents)

    def owners(totalRef: BlockId): Set[Channel] = Option(microBlockOwners.getIfPresent(totalRef)).getOrElse(MSet.empty).toSet

    def alreadyRequested(totalRef: MicroBlockSignature): Boolean = Option(awaiting.getIfPresent(totalRef)).isDefined

    def alreadyProcessed(totalRef: MicroBlockSignature): Boolean = Option(successfullyReceived.getIfPresent(totalRef)).isDefined

    val cacheSizesReporter = Coeval.eval {
      CacheSizes(microBlockOwners.size(), nextInvs.size(), awaiting.size(), successfullyReceived.size())
    }

    def requestMicroBlock(mbInv: MicroBlockInv): CancelableFuture[Unit] = {
      import mbInv.totalBlockId

      def randomOwner(exclude: Set[Channel]) = random(owners(mbInv.totalBlockId) -- exclude)

      def task(attemptsAllowed: Int, exclude: Set[Channel]): Task[Unit] = Task.unit.flatMap { _ =>
        if (attemptsAllowed <= 0 || alreadyProcessed(totalBlockId)) Task.unit
        else
          randomOwner(exclude).fold(Task.unit) { channel =>
            if (channel.isOpen) {
              val request = MicroBlockRequest(totalBlockId)
              channel.writeAndFlush(request)
              awaiting.put(totalBlockId, mbInv)
              task(attemptsAllowed - 1, exclude + channel).delayExecution(settings.waitResponseTimeout)
            } else task(attemptsAllowed, exclude + channel)
          }
      }

      task(MicroBlockDownloadAttempts, Set.empty).runAsyncLogErr
    }

    def tryDownloadNext(prevBlockId: ByteStr): Unit = Option(nextInvs.getIfPresent(prevBlockId)) match {
      case Some(inv) =>
        log.trace(s"Found $inv for $prevBlockId")
        requestMicroBlock(inv)
      case None =>
        log.trace(s"Could not find inv for $prevBlockId")
    }

    lastBlockIdEvents
      .mapEval { f =>
        log.trace(s"Last block id: $f")
        Task(tryDownloadNext(f))
      }
      .executeOn(scheduler)
      .logErr
      .subscribe()

    microblockInvs
      .mapEval {
        case (ch, mbInv @ MicroBlockInv(_, totalRef, prevRef, _)) =>
          log.trace(s"About to process $mbInv")
          Task {
            log.trace(s"Validating signatures in $mbInv")
            val sig = try mbInv.signaturesValid()
            catch {
              case t: Throwable =>
                log.error(s"Error validating signatures")
                throw t
            }
            sig match {
              case Left(err) =>
                peerDatabase.blacklistAndClose(ch, err.toString)
              case Right(_) =>
                log.trace(s"Signatures valid, now updating caches")
                microBlockOwners.get(totalRef, () => MSet.empty) += ch
                nextInvs.get(prevRef, { () =>
                  BlockStats.inv(mbInv, ch)
                  mbInv
                })
                lastBlockId() match {
                  case Some(blockId) if blockId == prevRef && !alreadyRequested(totalRef) =>
                    log.trace(s"About to download $blockId")
                    tryDownloadNext(blockId)
                  case other =>
                    log.trace(s"NOT downloading, lastBlockId=$other, prevRef=$prevRef, alreadyRequested=${alreadyRequested(totalRef)}")
                }
            }
          }.logErr
      }
      .executeOn(scheduler)
      .logErr
      .subscribe()

    val observable = microblockResponses.observeOn(scheduler).flatMap {
      case (ch, MicroBlockResponse(mb, totalRef)) =>
        successfullyReceived.put(totalRef, dummy)
        BlockStats.received(mb, ch)
        Option(awaiting.getIfPresent(totalRef)) match {
          case None => Observable.empty
          case Some(mi) =>
            awaiting.invalidate(totalRef)
            Observable((ch, MicroblockData(Option(mi), mb, Coeval.evalOnce(owners(totalRef)))))
        }
    }
    (observable, cacheSizesReporter)
  }

  case class MicroblockData(invOpt: Option[MicroBlockInv], microBlock: MicroBlock, microblockOwners: Coeval[Set[Channel]])

  type MicroBlockSignature = ByteStr

  private val MicroBlockDownloadAttempts = 2

  def random[T](s: Set[T]): Option[T] =
    if (s.isEmpty) None
    else {
      val n = util.Random.nextInt(s.size)
      s.drop(n).headOption
    }

  def cache[K <: AnyRef, V <: AnyRef](name: String, timeout: FiniteDuration): Cache[K, V] =
    CacheBuilder
      .newBuilder()
      .expireAfterWrite(timeout.toMillis, TimeUnit.MILLISECONDS)
      .removalListener { rn: RemovalNotification[K, V] =>
        log.trace(s"$name - REMOVED (${rn.getCause}, evicted=${rn.wasEvicted()}): ${rn.getKey} -> ${rn.getValue}")
      }
      .build[K, V]()

  case class CacheSizes(microBlockOwners: Long, nextInvs: Long, awaiting: Long, successfullyReceived: Long)

  private val dummy = new Object() {
    override def toString: String = "dummy stub"
  }
}
