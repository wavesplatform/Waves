package com.wavesplatform.network

import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.network.HistoryReplier._
import com.wavesplatform.network.MicroBlockSynchronizer.MicroBlockSignature
import com.wavesplatform.settings.SynchronizationSettings
import com.wavesplatform.state.NG
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import monix.eval.Task
import monix.execution.schedulers.SchedulerService

@Sharable
class HistoryReplier(ng: NG, settings: SynchronizationSettings, scheduler: SchedulerService) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private lazy val historyReplierSettings = settings.historyReplierSettings

  private implicit val s: SchedulerService = scheduler

  private val knownMicroBlocks = CacheBuilder
    .newBuilder()
    .maximumSize(historyReplierSettings.maxMicroBlockCacheSize)
    .build(new CacheLoader[MicroBlockSignature, Array[Byte]] {
      override def load(key: MicroBlockSignature) =
        ng.microBlock(key)
          .map(m => MicroBlockResponseSpec.serializeData(MicroBlockResponse(m)))
          .get
    })

  private val knownBlocks = CacheBuilder
    .newBuilder()
    .maximumSize(historyReplierSettings.maxBlockCacheSize)
    .build(new CacheLoader[ByteStr, Array[Byte]] {
      override def load(key: ByteStr) = ng.blockBytes(key).get
    })

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case GetSignatures(otherSigs) =>
      Task {
        val nextIds = otherSigs.view
          .map(id => id -> ng.blockIdsAfter(id, settings.maxChainLength))
          .collectFirst { case (parent, Some(ids)) => parent +: ids }

        nextIds match {
          case Some(extension) =>
            log.debug(
              s"${id(ctx)} Got GetSignatures with ${otherSigs.length}, found common parent ${extension.head} and sending total of ${extension.length} signatures")
            ctx.writeAndFlush(Signatures(extension))
          case None =>
            log.debug(s"${id(ctx)} Got GetSignatures with ${otherSigs.length} signatures, but could not find an extension")
        }
      }.runAsyncLogErr

    case GetBlock(sig) =>
      Task(knownBlocks.get(sig)).map(bytes => ctx.writeAndFlush(RawBytes(BlockSpec.messageCode, bytes))).logErrDiscardNoSuchElementException.runAsync

    case mbr @ MicroBlockRequest(totalResBlockSig) =>
      Task(knownMicroBlocks.get(totalResBlockSig))
        .map { bytes =>
          ctx.writeAndFlush(RawBytes(MicroBlockResponseSpec.messageCode, bytes))
          log.trace(id(ctx) + s"Sent MicroBlockResponse(total=${totalResBlockSig.trim})")
        }
        .logErrDiscardNoSuchElementException
        .runAsync

    case _: Handshake =>
      Task {
        if (ctx.channel().isOpen)
          ctx.writeAndFlush(LocalScoreChanged(ng.score))
      }.runAsyncLogErr

    case _ => super.channelRead(ctx, msg)
  }

  def cacheSizes: CacheSizes = CacheSizes(knownBlocks.size(), knownMicroBlocks.size())
}

object HistoryReplier {

  case class CacheSizes(blocks: Long, microBlocks: Long)

}
