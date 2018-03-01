package com.wavesplatform.network

import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.wavesplatform.network.HistoryReplier._
import com.wavesplatform.network.MicroBlockSynchronizer.MicroBlockSignature
import com.wavesplatform.settings.SynchronizationSettings
import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

@Sharable
class HistoryReplier(history: NgHistory, settings: SynchronizationSettings, scheduler: SchedulerService) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private lazy val historyReplierSettings = settings.historyReplierSettings

  private implicit val s: SchedulerService = scheduler

  private val knownMicroBlocks = CacheBuilder.newBuilder()
    .maximumSize(historyReplierSettings.maxMicroBlockCacheSize)
    .build(new CacheLoader[MicroBlockSignature, Array[Byte]] {
      override def load(key: MicroBlockSignature) =
        history.microBlock(key)
          .map(m => MicroBlockResponseSpec.serializeData(MicroBlockResponse(m))).get
    })

  private val knownBlocks = CacheBuilder.newBuilder()
    .maximumSize(historyReplierSettings.maxBlockCacheSize)
    .build(new CacheLoader[ByteStr, Array[Byte]] {
      override def load(key: ByteStr) = history.heightOf(key).flatMap(history.blockBytes).get
    })

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case GetSignatures(otherSigs) => Task {
      otherSigs.view
        .map(parent => parent -> history.blockIdsAfter(parent, settings.maxChainLength))
        .find(_._2.nonEmpty) match {
        case Some((parent, extension)) =>
          log.debug(s"${id(ctx)} Got GetSignatures with ${otherSigs.length}, found common parent $parent and sending ${extension.length} more signatures")
          ctx.writeAndFlush(Signatures(parent +: extension))
        case None if otherSigs.lengthCompare(1) == 0 && otherSigs.head == history.lastBlock.get.uniqueId =>
          // this is the special case when both nodes only have genesis block
          log.debug(s"${id(ctx)} Both local and remote nodes only have genesis block")
          ctx.writeAndFlush(Signatures(otherSigs))
        case _ =>
          log.debug(s"${id(ctx)} Got GetSignatures with ${otherSigs.length} signatures, but could not find an extension")
      }
    }.runAsyncLogErr

    case GetBlock(sig) => Task(knownBlocks.get(sig)).map(bytes =>
      ctx.writeAndFlush(RawBytes(BlockSpec.messageCode, bytes)))
      .logErrDiscardNoSuchElementException
      .runAsync

    case mbr@MicroBlockRequest(totalResBlockSig) =>
      Task(knownMicroBlocks.get(totalResBlockSig)).map { bytes =>
        ctx.writeAndFlush(RawBytes(MicroBlockResponseSpec.messageCode, bytes))
        log.trace(id(ctx) + s"Sent MicroBlockResponse(total=${totalResBlockSig.trim})")
      }.logErrDiscardNoSuchElementException
        .runAsync

    case _: Handshake => Task {
      if (ctx.channel().isOpen)
        ctx.writeAndFlush(LocalScoreChanged(history.score))
    }.runAsyncLogErr

    case _ => super.channelRead(ctx, msg)
  }

  def cacheSizes: CacheSizes = CacheSizes(knownBlocks.size(), knownMicroBlocks.size())
}

object HistoryReplier {

  case class CacheSizes(blocks: Long, microBlocks: Long)

}