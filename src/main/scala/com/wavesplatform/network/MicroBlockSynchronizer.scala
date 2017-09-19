package com.wavesplatform.network

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.metrics.BlockStats
import com.wavesplatform.network.MicroBlockSynchronizer._
import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import monix.eval.Task
import kamon.Kamon
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

import scala.collection.mutable.{Set => MSet}
import scala.concurrent.duration.FiniteDuration

@Sharable
class MicroBlockSynchronizer(settings: Settings, history: NgHistory) extends ChannelInboundHandlerAdapter with ScorexLogging {

  private implicit val scheduler = monix.execution.Scheduler.singleThread("microblock-synchronizer")

  private val awaitingMicroBlocks = cache[MicroBlockSignature, Object](settings.invCacheTimeout)
  private val knownMicroBlockOwners = cache[MicroBlockSignature, MSet[ChannelHandlerContext]](settings.invCacheTimeout)
  private val successfullyReceivedMicroBlocks = cache[MicroBlockSignature, Object](settings.processedMicroBlocksCacheTimeout)
  private val microBlockRecieveTime = cache[ByteStr, java.lang.Long](settings.invCacheTimeout)

  private def alreadyRequested(microBlockSig: MicroBlockSignature): Boolean = Option(awaitingMicroBlocks.getIfPresent(microBlockSig)).isDefined

  private def alreadyProcessed(microBlockSig: MicroBlockSignature): Boolean = Option(successfullyReceivedMicroBlocks.getIfPresent(microBlockSig)).isDefined

  def requestMicroBlockTask(microBlockSig: MicroBlockSignature, attemptsAllowed: Int): Task[Unit] = Task {
    if (attemptsAllowed > 0 && !alreadyProcessed(microBlockSig)) {
      val knownChannels = knownMicroBlockOwners.get(microBlockSig, () => MSet.empty)
      random(knownChannels) match {
        case Some(ctx) =>
          knownChannels -= ctx
          ctx.writeAndFlush(MicroBlockRequest(microBlockSig))
          awaitingMicroBlocks.put(microBlockSig, dummy)
          requestMicroBlockTask(microBlockSig, attemptsAllowed - 1)
            .delayExecution(settings.waitResponseTimeout)
        case None => Task.unit
      }
    } else Task.unit
  }.flatten


  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case mbr@MicroBlockResponse(mb) => Task {
      log.trace(id(ctx) + "Received " + mbr)
      knownMicroBlockOwners.invalidate(mb.totalResBlockSig)
      awaitingMicroBlocks.invalidate(mb.totalResBlockSig)
      successfullyReceivedMicroBlocks.put(mb.totalResBlockSig, dummy)

      Option(microBlockRecieveTime.getIfPresent(mb.totalResBlockSig)).foreach { created =>
        BlockStats.received(
          m = mb,
          from = ctx.channel().remoteAddress().asInstanceOf[InetSocketAddress],
          propagationTime = System.currentTimeMillis() - created
        )
        microBlockRecieveTime.invalidate(mb.totalResBlockSig)
        super.channelRead(ctx, msg)
      }
    }.runAsync
    case mi@MicroBlockInv(totalResBlockSig, prevResBlockSig) => Task {
      log.trace(id(ctx) + "Received " + mi)
      history.lastBlockId() match {
        case Some(lastBlockId) =>
          if (lastBlockId == prevResBlockSig) {
            microBlockRecieveTime.put(totalResBlockSig, System.currentTimeMillis())
            knownMicroBlockOwners.get(totalResBlockSig, () => MSet.empty) += ctx
            microBlockInvStats.increment()

            if (alreadyRequested(totalResBlockSig)) Task.unit
            else requestMicroBlockTask(totalResBlockSig, 2)
          } else {
            notLastMicroblockStats.increment()
            log.trace(s"Discarding $mi because it doesn't match last (micro)block")
            Task.unit
          }

        case None =>
          unknownMicroblockStats.increment()
          Task.unit
      }
    }.flatten.runAsync
    case _ => super.channelRead(ctx, msg)
  }
}

object MicroBlockSynchronizer {

  type MicroBlockSignature = ByteStr

  private val microBlockInvStats = Kamon.metrics.registerCounter("micro-inv")

  private val notLastMicroblockStats = Kamon.metrics.registerCounter("micro-not-last")
  private val unknownMicroblockStats = Kamon.metrics.registerCounter("micro-unknown")

  case class Settings(waitResponseTimeout: FiniteDuration,
                      processedMicroBlocksCacheTimeout: FiniteDuration,
                      invCacheTimeout: FiniteDuration)

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
