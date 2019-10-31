package com.wavesplatform.network

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.network.HistoryReplier._
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import monix.eval.Task

@Sharable
class HistoryReplier(
    score: => BigInt,
    loadBlockBytes: ByteStr => Task[Array[Byte]],
    loadMicroBlockBytes: ByteStr => Task[Array[Byte]],
    blockIdsAfter: Seq[ByteStr] => Task[Seq[ByteStr]]
) extends ChannelInboundHandlerAdapter
    with ScorexLogging {

  private def respondWith(ctx: ChannelHandlerContext, loader: Task[Message]): Unit = ???

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case GetSignatures(otherSigs) =>
      respondWith(ctx, blockIdsAfter(otherSigs).map(s => Signatures(s)))

    case GetBlock(sig) =>
      respondWith(ctx, loadBlockBytes(sig).map(bytes => RawBytes(BlockSpec.messageCode, bytes)))

    case MicroBlockRequest(totalResBlockSig) =>
      respondWith(ctx, loadMicroBlockBytes(totalResBlockSig).map(bytes => RawBytes(MicroBlockResponseSpec.messageCode, bytes)))

    case _: Handshake =>
      respondWith(ctx, Task(LocalScoreChanged(score)))

    case _ => super.channelRead(ctx, msg)
  }

  def cacheSizes: CacheSizes = CacheSizes(0, 0)
}

object HistoryReplier {
  case class CacheSizes(blocks: Long, microBlocks: Long)
}
