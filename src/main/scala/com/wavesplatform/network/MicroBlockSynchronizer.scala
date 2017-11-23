package com.wavesplatform.network

import java.util.concurrent.TimeUnit

import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.metrics.BlockStats
import com.wavesplatform.settings.SynchronizationSettings.MicroblockSynchronizerSettings
import com.wavesplatform.state2.ByteStr
import io.netty.channel._
import monix.eval.{Coeval, Task}
import monix.execution.schedulers.SchedulerService
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import scorex.block.Block.BlockId
import scorex.block.MicroBlock
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

import scala.collection.mutable.{Set => MSet}
import scala.concurrent.duration.FiniteDuration

object MicroBlockSynchronizer extends ScorexLogging {

  def apply(settings: MicroblockSynchronizerSettings,
            history: NgHistory,
            peerDatabase: PeerDatabase)
           (lastBlockIdEvents: Observable[ByteStr],
            microblockInvs: ChannelObservable[MicroBlockInv],
            microblockResponses: ChannelObservable[MicroBlockResponse]): ChannelObservable[MicroblockData] = {

    implicit val scheduler: SchedulerService = Scheduler.singleThread("microblock-synchronizer")

    val microBlockOwners = cache[MicroBlockSignature, MSet[Channel]](settings.invCacheTimeout)

    val nextInvs = cache[MicroBlockSignature, MicroBlockInv](settings.invCacheTimeout)
    val awaiting = cache[MicroBlockSignature, MicroBlockInv](settings.invCacheTimeout)
    val successfullyReceived = cache[MicroBlockSignature, Object](settings.processedMicroBlocksCacheTimeout)

    def owners(totalResBlockSig: BlockId): Set[Channel] = Option(microBlockOwners.getIfPresent(totalResBlockSig)).getOrElse(MSet.empty).toSet

    def alreadyRequested(totalSig: MicroBlockSignature): Boolean = Option(awaiting.getIfPresent(totalSig)).isDefined

    def alreadyProcessed(totalSig: MicroBlockSignature): Boolean = Option(successfullyReceived.getIfPresent(totalSig)).isDefined

    def requestMicroBlock(mbInv: MicroBlockInv): CancelableFuture[Unit] = {
      import mbInv.totalBlockSig

      def randomOwner(exclude: Set[Channel]) = random(owners(mbInv.totalBlockSig) -- exclude)

      def task(attemptsAllowed: Int, exclude: Set[Channel]): Task[Unit] = Task.unit.flatMap { _ =>
        if (attemptsAllowed <= 0 || alreadyProcessed(totalBlockSig)) Task.unit
        else randomOwner(exclude).fold(Task.unit) { channel =>
          if (channel.isOpen) {
            val request = MicroBlockRequest(totalBlockSig)
            channel.writeAndFlush(request)
            log.trace(s"${id(channel)} Sent $request")
            awaiting.put(totalBlockSig, mbInv)
            task(attemptsAllowed - 1, exclude + channel).delayExecution(settings.waitResponseTimeout)
          } else task(attemptsAllowed, exclude + channel)
        }
      }

      task(MicroBlockDownloadAttempts, Set.empty).runAsyncLogErr
    }

    def tryDownloadNext(prevBlockId: ByteStr): Unit = Option(nextInvs.getIfPresent(prevBlockId)).foreach(requestMicroBlock)

    lastBlockIdEvents.mapTask(f => Task(tryDownloadNext(f))).logErr.subscribe()

    microblockInvs.mapTask { case ((ch, mbInv@MicroBlockInv(_, totalSig, prevSig, _))) => Task {
      mbInv.signaturesValid() match {
        case Left(err) =>
          peerDatabase.blacklistAndClose(ch, err.toString)
        case Right(_) =>
          log.trace(s"${id(ch)} Received $mbInv")
          microBlockOwners.get(totalSig, () => MSet.empty) += ch
          nextInvs.get(prevSig, { () =>
            BlockStats.inv(mbInv, ch)
            mbInv
          })
          history.lastBlockId()
            .filter(_ == prevSig && !alreadyRequested(totalSig))
            .foreach(tryDownloadNext)
      }
    }
    }.logErr.subscribe()

    microblockResponses.flatMap { case ((ch, msg@MicroBlockResponse(mb))) =>
      import mb.{totalResBlockSig => totalSig}
      successfullyReceived.put(totalSig, dummy)
      BlockStats.received(mb, ch)
      log.trace(s"${id(ch)} Received $msg")
      Option(awaiting.getIfPresent(totalSig)) match {
        case None => Observable.empty
        case Some(mi) =>
          awaiting.invalidate(totalSig)
          Observable((ch, MicroblockData(Option(mi), mb, Coeval.evalOnce(owners(totalSig)))))
      }
    }.executeOn(scheduler)
  }

  case class MicroblockData(invOpt: Option[MicroBlockInv], microBlock: MicroBlock, microblockOwners: Coeval[Set[Channel]])

  type MicroBlockSignature = ByteStr

  private val MicroBlockDownloadAttempts = 2

  def random[T](s: Set[T]): Option[T] = if (s.isEmpty) None else {
    val n = util.Random.nextInt(s.size)
    s.drop(n).headOption
  }

  def cache[K <: AnyRef, V <: AnyRef](timeout: FiniteDuration): Cache[K, V] = CacheBuilder.newBuilder()
    .expireAfterWrite(timeout.toMillis, TimeUnit.MILLISECONDS)
    .build[K, V]()

  private val dummy = new Object()
}
