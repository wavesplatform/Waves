package com.wavesplatform.network

import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.google.common.util.concurrent.UncheckedExecutionException
import com.wavesplatform.block.{Block, MicroBlock}
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
  private lazy val historyReplierSettings = settings.historyReplier

  private implicit val s: SchedulerService = scheduler

  // both caches call .get so that NoSuchElementException is thrown and only successfully loaded (micro)blocks are cached

  private val knownMicroBlocks = CacheBuilder
    .newBuilder()
    .maximumSize(historyReplierSettings.maxMicroBlockCacheSize)
    .build(new CacheLoader[MicroBlockSignature, RawBytes] {
      override def load(key: MicroBlockSignature): RawBytes = RawBytes.fromMicroBlock(ng.microBlock(key).get)
    })

  private val knownBlocks = CacheBuilder
    .newBuilder()
    .maximumSize(historyReplierSettings.maxBlockCacheSize)
    .build(new CacheLoader[ByteStr, Block] {
      override def load(key: ByteStr): RawBytes = RawBytes.fromBlock(ng.blockById(key).get)
    })

  private def respondWith(ctx: ChannelHandlerContext, loader: Task[Option[Any]]): Unit =
    loader.map {
      case Some(msg) if ctx.channel().isOpen => ctx.writeAndFlush(msg)
      case _                                 =>
    }.runAsyncLogErr

  private def handlingNSE[A](f: => A): Option[A] =
    try Some(f)
    catch {
      case uee: UncheckedExecutionException if uee.getCause != null && uee.getCause.isInstanceOf[NoSuchElementException] => None
    }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case GetSignatures(otherSigs) =>
      respondWith(
        ctx,
        Task {
          val nextIds = otherSigs.view
            .map(id => id -> ng.blockIdsAfter(id, settings.maxChainLength))
            .collectFirst { case (parent, Some(ids)) => parent +: ids }

          nextIds match {
            case Some(extension) =>
              log.debug(
                s"${id(ctx)} Got GetSignatures with ${otherSigs.length}, found common parent ${extension.head} and sending total of ${extension.length} signatures"
              )
              Some(Signatures(extension))
            case None =>
              log.debug(s"${id(ctx)} Got GetSignatures with ${otherSigs.length} signatures, but could not find an extension")
              None
          }
        }
      )

    case GetBlock(sig) =>
      respondWith(ctx, Task(handlingNSE(knownBlocks.get(sig))))

    case MicroBlockRequest(totalResBlockSig) =>
      respondWith(ctx, Task(handlingNSE(knownMicroBlocks.get(totalResBlockSig))))

    case _: Handshake =>
      respondWith(ctx, Task(Some(LocalScoreChanged(ng.score))))

    case _ => super.channelRead(ctx, msg)
  }

  def cacheSizes: CacheSizes = CacheSizes(knownBlocks.size(), knownMicroBlocks.size())
}

object HistoryReplier {
  case class CacheSizes(blocks: Long, microBlocks: Long)
}
