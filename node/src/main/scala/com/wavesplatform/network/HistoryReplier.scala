package com.wavesplatform.network

import com.wavesplatform.block.Block
import com.wavesplatform.history.History
import com.wavesplatform.settings.SynchronizationSettings
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}

import scala.concurrent.ExecutionContext

@Sharable
class HistoryReplier(score: => BigInt, history: History, settings: SynchronizationSettings)(implicit ec: ExecutionContext)
    extends ChannelInboundHandlerAdapter
    with ScorexLogging {

  private def respondWith(ctx: ChannelHandlerContext, value: => Message | String): Unit = ec.execute { () =>
    value match {
      case msg: Message =>
        if (ctx.channel().isOpen) {
          ctx.writeAndFlush(msg)
        } else {
          log.trace(s"${id(ctx)} Channel is closed")
        }
      case notFoundMessage: String =>
        log.debug(s"${id(ctx)} $notFoundMessage")
    }
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case GetSignatures(otherSigs) =>
      respondWith(ctx, Signatures(history.blockIdsAfter(otherSigs, settings.maxRollback)))

    case GetBlock(sig) =>
      respondWith(
        ctx,
        history.loadBlockBytes(sig).fold(s"Error loading block $sig") { case (blockVersion, bytes) =>
          RawBytes(if (blockVersion < Block.ProtoBlockVersion) BlockSpec.messageCode else PBBlockSpec.messageCode, bytes)
        }
      )

    case MicroBlockRequest(microBlockId) =>
      respondWith(
        ctx,
        history.loadMicroBlock(microBlockId).fold(s"Error loading microblock $microBlockId") { microBlock =>
          RawBytes.fromMicroBlock(MicroBlockResponse(microBlock, microBlockId))
        }
      )

    case GetSnapshot(id) =>
      respondWith(
        ctx,
        history.loadBlockSnapshots(id).fold(s"Error loading snapshots for block $id") { snapshots => BlockSnapshotResponse(id, snapshots) }
      )

    case MicroSnapshotRequest(id) =>
      respondWith(
        ctx,
        history.loadMicroBlockSnapshots(id).fold(s"Error loading snapshots for microblock $id") { snapshots =>
          MicroBlockSnapshotResponse(id, snapshots)
        }
      )

    case _: Handshake =>
      respondWith(ctx, LocalScoreChanged(score))

    case _ => super.channelRead(ctx, msg)
  }

  def cacheSizes: HistoryReplier.CacheSizes = HistoryReplier.CacheSizes(0, 0)
}

object HistoryReplier {
  case class CacheSizes(blocks: Long, microBlocks: Long)
}
