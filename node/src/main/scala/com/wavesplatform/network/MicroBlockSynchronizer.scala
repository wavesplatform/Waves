package com.wavesplatform.network

import java.util.concurrent.TimeUnit

import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.MicroBlock
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.metrics.BlockStats
import com.wavesplatform.settings.SynchronizationSettings.MicroblockSynchronizerSettings
import io.netty.channel._
import monix.eval.{Coeval, Task}
import monix.execution.CancelableFuture
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable

import scala.collection.mutable.{Set => MSet}
import scala.concurrent.duration.FiniteDuration

object MicroBlockSynchronizer {

  def apply(settings: MicroblockSynchronizerSettings,
            peerDatabase: PeerDatabase,
            lastBlockIdEvents: Observable[ByteStr],
            microblockInvs: ChannelObservable[MicroBlockInv],
            microblockResponses: ChannelObservable[MicroBlockResponse],
            scheduler: SchedulerService): (Observable[(Channel, MicroblockData)], Coeval[CacheSizes]) = {

    implicit val schdlr: SchedulerService = scheduler

    val microBlockOwners     = cache[MicroBlockSignature, MSet[Channel]](settings.invCacheTimeout)
    val nextInvs             = cache[MicroBlockSignature, MicroBlockInv](settings.invCacheTimeout)
    val awaiting             = cache[MicroBlockSignature, MicroBlockInv](settings.invCacheTimeout)
    val successfullyReceived = cache[MicroBlockSignature, Object](settings.processedMicroBlocksCacheTimeout)

    val lastBlockId = lastObserved(lastBlockIdEvents)

    def owners(totalRef: BlockId): Set[Channel] = Option(microBlockOwners.getIfPresent(totalRef)).getOrElse(MSet.empty).toSet

    def alreadyRequested(totalRef: MicroBlockSignature): Boolean = Option(awaiting.getIfPresent(totalRef)).isDefined

    def alreadyProcessed(totalRef: MicroBlockSignature): Boolean = Option(successfullyReceived.getIfPresent(totalRef)).isDefined

    val cacheSizesReporter = Coeval.eval {
      CacheSizes(microBlockOwners.size(), nextInvs.size(), awaiting.size(), successfullyReceived.size())
    }

    def requestMicroBlock(mbInv: MicroBlockInv): CancelableFuture[Unit] = {
      import mbInv.totalBlockRef

      def randomOwner(exclude: Set[Channel]) = random(owners(mbInv.totalBlockRef) -- exclude)

      def task(attemptsAllowed: Int, exclude: Set[Channel]): Task[Unit] = Task.unit.flatMap { _ =>
        if (attemptsAllowed <= 0 || alreadyProcessed(totalBlockRef)) Task.unit
        else
          randomOwner(exclude).fold(Task.unit) { channel =>
            if (channel.isOpen) {
              val request = MicroBlockRequest(totalBlockRef)
              channel.writeAndFlush(request)
              awaiting.put(totalBlockRef, mbInv)
              task(attemptsAllowed - 1, exclude + channel).delayExecution(settings.waitResponseTimeout)
            } else task(attemptsAllowed, exclude + channel)
          }
      }

      task(MicroBlockDownloadAttempts, Set.empty).runAsyncLogErr
    }

    def tryDownloadNext(prevBlockId: ByteStr): Unit = Option(nextInvs.getIfPresent(prevBlockId)).foreach(requestMicroBlock)

    lastBlockIdEvents.mapEval(f => Task(tryDownloadNext(f))).executeOn(scheduler).logErr.subscribe()

    microblockInvs
      .mapEval {
        case (ch, mbInv @ MicroBlockInv(_, totalRef, prevRef, _)) =>
          Task {
            mbInv.signaturesValid() match {
              case Left(err) =>
                peerDatabase.blacklistAndClose(ch, err.toString)
              case Right(_) =>
                microBlockOwners.get(totalRef, () => MSet.empty) += ch
                nextInvs.get(prevRef, { () =>
                  BlockStats.inv(mbInv, ch)
                  mbInv
                })
                lastBlockId()
                  .filter(_ == prevRef && !alreadyRequested(totalRef))
                  .foreach(tryDownloadNext)
            }
          }
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

  def cache[K <: AnyRef, V <: AnyRef](timeout: FiniteDuration): Cache[K, V] =
    CacheBuilder
      .newBuilder()
      .expireAfterWrite(timeout.toMillis, TimeUnit.MILLISECONDS)
      .build[K, V]()

  case class CacheSizes(microBlockOwners: Long, nextInvs: Long, awaiting: Long, successfullyReceived: Long)

  private val dummy = new Object()
}
