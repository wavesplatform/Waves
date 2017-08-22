package com.wavesplatform.network

import com.wavesplatform.job._
import com.wavesplatform.network.MicroBlockSynchronizer.{Item, QueuedRequests, Settings}
import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration

@Sharable
class MicroBlockSynchronizer(settings: Settings, history: NgHistory)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private val requests = new CachedParallelJobQueue(new QueuedRequests(settings.waitResponseTimeout))

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case MicroBlockResponse(mb) =>
      log.trace(id(ctx) + "Received MicroBlockResponse " + mb)
      requests.shutdownGroup(Item(ctx, mb.signature))

    case mi @ MicroBlockInv(totalResBlockSig, prevResBlockSig) =>
      log.trace(id(ctx) + "Received " + mi)
      history.lastBlockId() match {
        case Some(lastBlockId) =>
          if (lastBlockId == prevResBlockSig) {
            requests.enqueue(Item(ctx, totalResBlockSig))
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

  case class Settings(waitResponseTimeout: FiniteDuration)

  case class Item(ctx: ChannelHandlerContext,
                  microBlockSig: ByteStr)

  case class Request(item: Item) extends Runnable {
    val id: String = s"${item.microBlockSig}.${item.ctx.channel().id().asLongText()}"

    override def run(): Unit = item.ctx.writeAndFlush(MicroBlockRequest(item.microBlockSig))
  }

  class QueuedRequests(waitResponseTimeout: FiniteDuration) extends ParallelJobQueue[Item, ByteStr, Request] {
    // A possible issue: it has an unbounded queue size
    private val scheduler = monix.execution.Scheduler.singleThread("queued-requests")

    override def groupId(item: Item): ByteStr = item.microBlockSig
    override def newJob(item: Item): Request = Request(item)
    override def newJobQueue: JobQueue[Request] = {
      val orig = new DelayedJobQueue[Request](waitResponseTimeout)(scheduler)
      new CachedJobQueue(orig, _.id)
    }
  }

}
