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

    def owners(totalResBlockSig: BlockId): Set[Channel] = Option(microBlockOwners.getIfPresent(totalResBlockSig)).getOrElse(MSet.empty).toSet

    def alreadyRequested(totalSig: MicroBlockSignature): Boolean = Option(awaiting.getIfPresent(totalSig)).isDefined

    def alreadyProcessed(totalSig: MicroBlockSignature): Boolean = Option(successfullyReceived.getIfPresent(totalSig)).isDefined

    val cacheSizesReporter = Coeval.eval {
      CacheSizes(microBlockOwners.size(), nextInvs.size(), awaiting.size(), successfullyReceived.size())
    }

    def requestMicroBlock(mbInv: MicroBlockInv): CancelableFuture[Unit] = {
      import mbInv.totalBlockSig

      def randomOwner(exclude: Set[Channel]) = random(owners(mbInv.totalBlockSig) -- exclude)

      def task(attemptsAllowed: Int, exclude: Set[Channel]): Task[Unit] = Task.unit.flatMap { _ =>
        if (attemptsAllowed <= 0 || alreadyProcessed(totalBlockSig)) Task.unit
        else
          randomOwner(exclude).fold(Task.unit) { channel =>
            if (channel.isOpen) {
              val request = MicroBlockRequest(totalBlockSig)
              channel.writeAndFlush(request)
              awaiting.put(totalBlockSig, mbInv)
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
        case (ch, mbInv @ MicroBlockInv(_, totalSig, prevSig, _)) =>
          Task {
            mbInv.signaturesValid() match {
              case Left(err) =>
                peerDatabase.blacklistAndClose(ch, err.toString)
              case Right(_) =>
                microBlockOwners.get(totalSig, () => MSet.empty) += ch
                nextInvs.get(prevSig, { () =>
                  BlockStats.inv(mbInv, ch)
                  mbInv
                })
                lastBlockId()
                  .filter(_ == prevSig && !alreadyRequested(totalSig))
                  .foreach(tryDownloadNext)
            }
          }
      }
      .executeOn(scheduler)
      .logErr
      .subscribe()

    val observable = microblockResponses.observeOn(scheduler).flatMap {
      case (ch, MicroBlockResponse(mb)) =>
        import mb.{totalResBlockSig => totalSig}
        successfullyReceived.put(totalSig, dummy)
        BlockStats.received(mb, ch)
        Option(awaiting.getIfPresent(totalSig)) match {
          case None => Observable.empty
          case Some(mi) =>
            awaiting.invalidate(totalSig)
            Observable((ch, MicroblockData(Option(mi), mb, Coeval.evalOnce(owners(totalSig)))))
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
