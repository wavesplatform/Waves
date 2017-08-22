package com.wavesplatform.network

import com.wavesplatform.job._
import com.wavesplatform.network.MicroBlockSynchronizer._
import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration

@Sharable
class MicroBlockSynchronizer(settings: Settings, history: NgHistory)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private val requests: ParallelJobPool[Item, MicroBlockSignature, SynchronizerRequest] = {
    val orig = new QueuedRequests(settings.waitResponseTimeout)

    // Prevents processing of the same microblock in gap between MicroBlockSynchronizer and CoordinatorHandler
    val ignoreStaleGroups = ParallelJobPool.ignoreStaleGroups(settings.processedMicroBlocksCacheTimeout, orig)

    val cached = ParallelJobPool.cached(settings.invCacheTimeout, ignoreStaleGroups) { x =>
      s"${x.microBlockSig}.${x.ctx.channel().id().asLongText()}"
    }

    cached
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case MicroBlockResponse(mb) =>
      log.trace(id(ctx) + "Received MicroBlockResponse " + mb)
      requests.shutdownPoolOf(Item(ctx, mb.signature))

    case mi @ MicroBlockInv(totalResBlockSig, prevResBlockSig) =>
      log.trace(id(ctx) + "Received " + mi)
      history.lastBlockId() match {
        case Some(lastBlockId) =>
          if (lastBlockId == prevResBlockSig) {
            requests.add(Item(ctx, totalResBlockSig))
          } else {
            log.trace(s"Discarding $mi because it doesn't match last (micro)block")
          }

        case None =>
          log.warn("History does not contain the last block!")
      }

    case _ => super.channelRead(ctx, msg)
  }
}

object MicroBlockSynchronizer {

  type MicroBlockSignature = ByteStr

  case class Settings(waitResponseTimeout: FiniteDuration,
                      processedMicroBlocksCacheTimeout: FiniteDuration,
                      invCacheTimeout: FiniteDuration)

  case class Item(ctx: ChannelHandlerContext,
                  microBlockSig: MicroBlockSignature)

  case class SynchronizerRequest(item: Item) extends Runnable {
    override def run(): Unit = item.ctx.writeAndFlush(MicroBlockRequest(item.microBlockSig))
  }

  class QueuedRequests(waitResponseTimeout: FiniteDuration)
    extends ParallelJobPool[Item, MicroBlockSignature, SynchronizerRequest] {

    // A possible issue: it has an unbounded queue size
    private val scheduler = monix.execution.Scheduler.singleThread("queued-requests")

    override def groupId(item: Item): MicroBlockSignature = item.microBlockSig
    override def newJob(item: Item): SynchronizerRequest = SynchronizerRequest(item)
    override def newJobPool: JobPool[SynchronizerRequest] = {
      new DelayQueueJobPool[SynchronizerRequest](waitResponseTimeout)(scheduler)
    }
  }

}
