package com.wavesplatform.network

import java.util.concurrent.locks.ReentrantReadWriteLock

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.block.Block.BlockId
import scorex.transaction.NgHistory
import scorex.utils.{ScorexLogging, Synchronized}

@Sharable
class MircoBlockSynchronizer(history: NgHistory)
  extends ChannelInboundHandlerAdapter with ScorexLogging with Synchronized {

  val synchronizationToken = new ReentrantReadWriteLock()
  private val awaitingResponse = Synchronized(Map.empty[BlockId, Channel])

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case MicroBlockInv(mid) =>
      history.microBlock(mid) match {
        case Some(_) => // already exists
        case None =>
          write { implicit l =>
            val snapshot = awaitingResponse()
            snapshot.get(mid) match {
              case None =>
                awaitingResponse.set(snapshot + (mid -> ctx.channel()))
                ctx.writeAndFlush(MicroBlockRequest(mid))
              case _ => // already requested
            }

          }
      }
    case _ => super.channelRead(ctx, msg)
  }
}
