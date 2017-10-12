package com.wavesplatform.network

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.metrics.BlockStats
import com.wavesplatform.network.MicroBlockSynchronizer._
import com.wavesplatform.settings.SynchronizationSettings.MicroblockSynchronizerSettings
import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import monix.eval.Task
import kamon.Kamon
import scorex.block.MicroBlock
import monix.reactive.Observable
import scorex.transaction.{NgHistory, Signed}
import scorex.utils.ScorexLogging

import scala.collection.mutable.{Set => MSet}
import scala.concurrent.duration.FiniteDuration

@Sharable
class MicroBlockSynchronizer(settings: MicroblockSynchronizerSettings,
                             history: NgHistory,
                             peerDatabase: PeerDatabase,
                             lastBlockIdEvents: Observable[ByteStr]) extends ChannelInboundHandlerAdapter with ScorexLogging {

  private implicit val scheduler = monix.execution.Scheduler.singleThread("microblock-synchronizer", reporter = com.wavesplatform.utils.UncaughtExceptionsToLogReporter)

  private val awaitingMicroBlocks = cache[MicroBlockSignature, MicroBlockInv](settings.invCacheTimeout)
  private val knownMicroBlockOwners = cache[MicroBlockSignature, MSet[ChannelHandlerContext]](settings.invCacheTimeout)
  private val knownNextMicroBlocks = cache[MicroBlockSignature, MicroBlockInv](settings.nextInvCacheTimeout)
  private val successfullyReceivedMicroBlocks = cache[MicroBlockSignature, Object](settings.processedMicroBlocksCacheTimeout)
  private val microBlockReceiveTime = cache[MicroBlockSignature, java.lang.Long](settings.invCacheTimeout)
  private val downloading = new AtomicBoolean(false)
  lastBlockIdEvents.foreach { lastBlockSig =>
    tryDownloadNext(lastBlockSig).runAsync
  }

  private def alreadyRequested(microBlockSig: MicroBlockSignature): Boolean = Option(awaitingMicroBlocks.getIfPresent(microBlockSig)).isDefined

  private def alreadyProcessed(microBlockSig: MicroBlockSignature): Boolean = Option(successfullyReceivedMicroBlocks.getIfPresent(microBlockSig)).isDefined

  private def requestMicroBlockTask(microblockInv: MicroBlockInv, attemptsAllowed: Int): Task[Unit] = Task.unit.flatMap { _ =>
    val totalResBlockSig = microblockInv.totalBlockSig
    if (attemptsAllowed > 0 && !alreadyProcessed(totalResBlockSig)) {
      val knownChannels = knownMicroBlockOwners.get(totalResBlockSig, () => MSet.empty)
      random(knownChannels) match {
        case Some(ctx) =>
          knownChannels -= ctx
          ctx.writeAndFlush(MicroBlockRequest(totalResBlockSig))
          awaitingMicroBlocks.put(totalResBlockSig, microblockInv)
          requestMicroBlockTask(microblockInv, attemptsAllowed - 1)
            .delayExecution(settings.waitResponseTimeout)
        case None => Task.unit
      }
    } else Task.unit
  }

  private def tryDownloadNext(lastBlockId: ByteStr): Task[Unit] = Task.unit.flatMap { _ =>
    if (downloading.compareAndSet(false, true)) {
      Option(knownNextMicroBlocks.getIfPresent(lastBlockId)).fold(Task.unit)(requestMicroBlockTask(_, 2))
    } else Task.unit
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case mbr@MicroBlockResponse(mb) =>
      downloading.set(false)
      Task {
        log.trace(id(ctx) + "Received " + mbr)
        knownMicroBlockOwners.invalidate(mb.totalResBlockSig)
        successfullyReceivedMicroBlocks.put(mb.totalResBlockSig, dummy)

        Option(microBlockReceiveTime.getIfPresent(mb.totalResBlockSig)) match {
          case Some(created) =>
            BlockStats.received(mb, ctx, propagationTime = System.currentTimeMillis() - created)
            microBlockReceiveTime.invalidate(mb.totalResBlockSig)
            super.channelRead(ctx, MicroblockData(Option(awaitingMicroBlocks.getIfPresent(mb.totalResBlockSig)), mb))
          case None =>
            BlockStats.received(mb, ctx)
        }
      }.runAsync
    case mi@MicroBlockInv(_, totalResBlockSig, prevResBlockSig, _) => Task.unit.flatMap { _ =>
      Signed.validateSignatures(mi) match {
        case Left(err) => Task.now(peerDatabase.blacklistAndClose(ctx.channel(), err.toString))
        case Right(_) =>
          log.trace(id(ctx) + "Received " + mi)
          history.lastBlockId() match {
            case Some(lastBlockId) =>
              knownNextMicroBlocks.get(mi.prevBlockSig, { () =>
                BlockStats.inv(mi, ctx)
                mi
              })
              knownMicroBlockOwners.get(totalResBlockSig, () => MSet.empty) += ctx

              if (lastBlockId == prevResBlockSig) {
                microBlockReceiveTime.put(totalResBlockSig, System.currentTimeMillis())
                microBlockInvStats.increment()

                if (alreadyRequested(totalResBlockSig)) Task.unit
                else tryDownloadNext(prevResBlockSig)
              } else {
                notLastMicroblockStats.increment()
                log.trace(s"Discarding $mi because it doesn't match last (micro)block")
                Task.unit
              }

            case None =>
              unknownMicroblockStats.increment()
              Task.unit
          }
      }
    }.runAsync
    case _ => super.channelRead(ctx, msg)
  }
}

object MicroBlockSynchronizer {

  case class MicroblockData(invOpt: Option[MicroBlockInv], microBlock: MicroBlock)

  type MicroBlockSignature = ByteStr

  private val microBlockInvStats = Kamon.metrics.registerCounter("micro-inv")

  private val notLastMicroblockStats = Kamon.metrics.registerCounter("micro-not-last")
  private val unknownMicroblockStats = Kamon.metrics.registerCounter("micro-unknown")

  def random[T](s: MSet[T]): Option[T] = {
    val n = util.Random.nextInt(s.size)
    val ts = s.iterator.drop(n)
    if (ts.hasNext) Some(ts.next)
    else None
  }

  def cache[K <: AnyRef, V <: AnyRef](timeout: FiniteDuration): Cache[K, V] = CacheBuilder.newBuilder()
    .expireAfterWrite(timeout.toMillis, TimeUnit.MILLISECONDS)
    .build[K, V]()

  private val dummy = new Object()
}
