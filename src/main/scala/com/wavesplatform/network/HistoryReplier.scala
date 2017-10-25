package com.wavesplatform.network

import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.network.MicroBlockSynchronizer.MicroBlockSignature
import com.wavesplatform.settings.SynchronizationSettings
import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

@Sharable
class HistoryReplier(history: NgHistory, settings: SynchronizationSettings) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private lazy val historyReplierSettings = settings.historyReplierSettings

  private val knownMicroBlocks: Cache[MicroBlockSignature, Array[Byte]] = CacheBuilder.newBuilder()
    .maximumSize(historyReplierSettings.maxMicroBlockCacheSize)
    .build[MicroBlockSignature, Array[Byte]]()

  private val knownBlocks: Cache[ByteStr, Array[Byte]] = CacheBuilder.newBuilder()
    .maximumSize(historyReplierSettings.maxBlockCacheSize)
    .build[ByteStr, Array[Byte]]()

  private def cachedBytes(cache: Cache[ByteStr, Array[Byte]], id: ByteStr, loader: => Option[Array[Byte]]): Option[Array[Byte]] = {
    Option(cache.getIfPresent(id)).orElse(loader.map(bytes => cache.get(id, () => bytes)))
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case GetSignatures(otherSigs) =>
      otherSigs.view
        .map(parent => parent -> history.blockIdsAfter(parent, settings.maxChainLength))
        .find(_._2.nonEmpty) match {
        case Some((parent, extension)) =>
          log.debug(s"${id(ctx)} Got GetSignatures with ${otherSigs.length}, found common parent $parent and sending ${extension.length} more signatures")
          ctx.writeAndFlush(Signatures(parent +: extension))
        case None if otherSigs.length == 1 && otherSigs.head == history.lastBlock.get.uniqueId =>
          // this is the special case when both nodes only have genesis block
          log.debug(s"${id(ctx)} Both local and remote nodes only have genesis block")
          ctx.writeAndFlush(Signatures(otherSigs))
        case _ =>
          log.debug(s"${id(ctx)} Got GetSignatures with ${otherSigs.length} signatures, but could not find an extension")
      }

    case GetBlock(sig) =>
      cachedBytes(knownBlocks, sig, history.heightOf(sig).flatMap(history.blockBytes)) match {
        case Some(bytes) => ctx.writeAndFlush(RawBytes(BlockMessageSpec.messageCode, bytes))
        case None => log.trace(s"Does not have a block $sig")
      }

    case mbr@MicroBlockRequest(totalResBlockSig) =>
      log.trace(id(ctx) + "Received " + mbr)
      cachedBytes(knownMicroBlocks, totalResBlockSig,
        history.microBlock(totalResBlockSig).map(m => MicroBlockResponseMessageSpec.serializeData(MicroBlockResponse(m)))).foreach { bytes =>
        ctx.writeAndFlush(RawBytes(MicroBlockResponseMessageSpec.messageCode, bytes))
        log.trace(id(ctx) + s"Sent MicroBlockResponse(total=${totalResBlockSig.trim})")
      }

    case _: Handshake =>
      ctx.writeAndFlush(LocalScoreChanged(history.score()))

    case _ => super.channelRead(ctx, msg)
  }
}
