package com.wavesplatform.network

import com.google.common.cache.{CacheBuilder, CacheLoader}
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
class HistoryReplier(history: NgHistory, settings: SynchronizationSettings) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private lazy val historyReplierSettings = settings.historyReplierSettings

  private implicit val scheduler: SchedulerService = monix.execution.Scheduler.fixedPool(
    name = "history-replier",
    reporter = com.wavesplatform.utils.UncaughtExceptionsToLogReporter,
    poolSize = 2
  )

  private val knownMicroBlocks = CacheBuilder.newBuilder()
    .maximumSize(historyReplierSettings.maxMicroBlockCacheSize)
    .build(new CacheLoader[MicroBlockSignature, Array[Byte]] {
      override def load(key: MicroBlockSignature) =
        history.microBlock(key)
          .map(m => MicroBlockResponseMessageSpec.serializeData(MicroBlockResponse(m))).get
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
        case None if otherSigs.length == 1 && otherSigs.head == history.lastBlock.get.uniqueId =>
          // this is the special case when both nodes only have genesis block
          log.debug(s"${id(ctx)} Both local and remote nodes only have genesis block")
          ctx.writeAndFlush(Signatures(otherSigs))
        case _ =>
          log.debug(s"${id(ctx)} Got GetSignatures with ${otherSigs.length} signatures, but could not find an extension")
      }
    }.runAsync

    case GetBlock(sig) => Task(knownBlocks.get(sig)).map(bytes =>
      ctx.writeAndFlush(RawBytes(BlockMessageSpec.messageCode, bytes)))
      .runAsync

    case mbr@MicroBlockRequest(totalResBlockSig) =>
      log.trace(id(ctx) + "Received " + mbr)
      Task(knownMicroBlocks.get(totalResBlockSig)).map { bytes =>
        ctx.writeAndFlush(RawBytes(MicroBlockResponseMessageSpec.messageCode, bytes))
        log.trace(id(ctx) + s"Sent MicroBlockResponse(total=${totalResBlockSig.trim})")
      }.runAsync

    case _: Handshake => Task {
      ctx.writeAndFlush(LocalScoreChanged(history.score()))
    }.runAsync

    case _ => super.channelRead(ctx, msg)
  }
}
