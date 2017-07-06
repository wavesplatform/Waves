package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{Channel, ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.block.Block.BlockId
import scorex.transaction.NgHistory
import scorex.utils.{ScorexLogging, SynchronizedOne}

@Sharable
class MircoBlockSynchronizer(history: NgHistory)
  extends ChannelInboundHandlerAdapter with ScorexLogging with SynchronizedOne {

  private val awaitingResponse = Synchronized(scala.collection.mutable.Map.empty[BlockId, Channel])

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case MicroBlockInv(mid) =>
      history.microBlock(mid) match {
        case Some(_) => // already exists
        case None =>
          write { implicit l =>
            awaitingResponse().get(mid) match {
              case None =>
                awaitingResponse.mutate(_ += (mid -> ctx.channel()))
                ctx.writeAndFlush(MicroBlockRequest(mid))
              case _ => // already requested
            }

          }
      }
    case _ => super.channelRead(ctx, msg)
  }
}
