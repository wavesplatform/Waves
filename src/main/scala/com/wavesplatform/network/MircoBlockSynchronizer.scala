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
    case MicroBlockResponse(mb) =>
      log.trace(id(ctx) + "Received MicroBlockResponse " + mb)
      write { implicit l =>
        awaitingResponse.mutate(_ -= mb.totalResBlockSig)
      }

    case mi@MicroBlockInv(totalResBlockSig, prevResBlockSig) =>
      log.trace(id(ctx) + "Received " + mi)
      history.lastBlockId() match {
        case Some(lbid) =>
          if (lbid == prevResBlockSig) {
            write { implicit l =>
              awaitingResponse().get(totalResBlockSig) match {
                case None =>
                  awaitingResponse.mutate(_ += (totalResBlockSig -> ctx.channel()))
                  ctx.writeAndFlush(MicroBlockRequest(totalResBlockSig))
                case _ => // already requested
              }
            }
          }
          else {
            log.trace(s"Discarding $mi because it doesn't match last (micro)block")
          }
        case None => ??? // Shouldn't happen
      }

    case _ => super.channelRead(ctx, msg)
  }
}
