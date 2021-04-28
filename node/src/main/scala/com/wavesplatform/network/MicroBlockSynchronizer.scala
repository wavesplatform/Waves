package com.wavesplatform.network

import java.util.concurrent.TimeUnit

import com.google.common.cache.{Cache, CacheBuilder}
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
      import mbInv.totalBlockId

      def randomOwner(exclude: Set[Channel]) = random(owners(mbInv.totalBlockId) -- exclude)

      def task(attemptsAllowed: Int, exclude: Set[Channel]): Task[Unit] = Task.defer {
        if (attemptsAllowed <= 0) {
          log.trace(s"No more attempts left to download $totalBlockId")
          Task.unit
        } else if (alreadyProcessed(totalBlockId)) {
          Task.unit
        } else
          randomOwner(exclude) match {
            case None =>
              log.trace(s"No owners found for $totalBlockId")
              Task.unit
            case Some(channel) =>
              if (channel.isOpen) {
                log.trace(s"${id(channel)} Requesting $totalBlockId")
                val request = MicroBlockRequest(totalBlockId)
                awaiting.put(totalBlockId, mbInv)
                channel.writeAndFlush(request)
                task(attemptsAllowed - 1, exclude + channel).delayExecution(settings.waitResponseTimeout)
              } else task(attemptsAllowed, exclude + channel)
          }
      }

      task(MicroBlockDownloadAttempts, Set.empty).runAsyncLogErr
    }

    def tryDownloadNext(prevBlockId: ByteStr): Unit = Option(nextInvs.getIfPresent(prevBlockId)).foreach(requestMicroBlock)

    lastBlockIdEvents
      .mapEval { f =>
        log.trace(s"Last block id is now $f")
        Task(tryDownloadNext(f))
      }
      .executeOn(scheduler)
      .logErr
      .subscribe()

    microblockInvs
      .mapEval {
        case (ch, mbInv @ MicroBlockInv(_, totalBlockId, reference, _)) =>
          Task.evalAsync {
            val sig = try mbInv.signaturesValid()
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
                nextInvs.get(reference, { () =>
                  BlockStats.inv(mbInv, ch)
                  mbInv
                })
                lastBlockId() match {
                  case Some(`reference`) if !alreadyRequested(totalBlockId) => tryDownloadNext(reference)
                  case _                                                    => // either the microblock has already been requested or it does no reference the last block
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
          case None =>
            log.trace(s"${id(ch)} Got unexpected ${mb.stringRepr(totalRef)}")
            Observable.empty
          case Some(mi) =>
            log.trace(s"${id(ch)} Got ${mb.stringRepr(totalRef)}, as expected")
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
