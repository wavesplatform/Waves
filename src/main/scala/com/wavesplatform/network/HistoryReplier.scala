package com.wavesplatform.network

import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.wavesplatform.network.MicroBlockSynchronizer.MicroBlockSignature
import com.wavesplatform.settings.SynchronizationSettings
import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}
import scala.util.{Failure, Success}

@Sharable
class HistoryReplier(history: NgHistory, settings: SynchronizationSettings) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private lazy val historyReplierSettings = settings.historyReplierSettings

  // both cache loaders will throw a NoSuchElementException when (micro)block is absent, but it's fine, because:
  // 1. cache reads are wrapped in a Future
  // 2. neither MicroBlockRequest nor GetBlock require a response when the requested (micro)block can not be found

  private val knownMicroBlocks = CacheBuilder.newBuilder()
    .maximumSize(historyReplierSettings.maxMicroBlockCacheSize)
    .build(new CacheLoader[MicroBlockSignature, Array[Byte]] {
      override def load(key: MicroBlockSignature) =
        blocking(history.microBlock(key))
          .map(m => MicroBlockResponseMessageSpec.serializeData(MicroBlockResponse(m))).get
    })

  private val knownBlocks = CacheBuilder.newBuilder()
    .maximumSize(historyReplierSettings.maxBlockCacheSize)
    .build(new CacheLoader[ByteStr, Array[Byte]] {
      override def load(key: ByteStr) = blocking(history.heightOf(key).flatMap(history.blockBytes)).get
    })

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case GetSignatures(otherSigs) => Future {
      otherSigs.view
        .map(parent => parent -> blocking(history.blockIdsAfter(parent, settings.maxChainLength)))
        .find(_._2.nonEmpty) match {
        case Some((parent, extension)) =>
          log.debug(s"${id(ctx)} Got GetSignatures with ${otherSigs.length}, found common parent $parent and sending ${extension.length} more signatures")
          ctx.writeAndFlush(Signatures(parent +: extension))
        case None if otherSigs.length == 1 && otherSigs.head == blocking(history.lastBlock).get.uniqueId =>
          // this is the special case when both nodes only have genesis block
          log.debug(s"${id(ctx)} Both local and remote nodes only have genesis block")
          ctx.writeAndFlush(Signatures(otherSigs))
        case _ =>
          log.debug(s"${id(ctx)} Got GetSignatures with ${otherSigs.length} signatures, but could not find an extension")
      }
    }

    case GetBlock(sig) =>
      Future(knownBlocks.get(sig)).foreach { bytes =>
        ctx.writeAndFlush(RawBytes(BlockMessageSpec.messageCode, bytes))
      }

    case mbr@MicroBlockRequest(totalResBlockSig) =>
      log.trace(id(ctx) + "Received " + mbr)

      Future(knownMicroBlocks.get(totalResBlockSig)).foreach { bytes =>
        ctx.writeAndFlush(RawBytes(MicroBlockResponseMessageSpec.messageCode, bytes))
        log.trace(id(ctx) + s"Sent MicroBlockResponse(total=${totalResBlockSig.trim})")
      }

    case _: Handshake =>
      Future(blocking(history.score())).onComplete {
        case Success(score) => ctx.writeAndFlush(LocalScoreChanged(score))
        case Failure(e) => ctx.fireExceptionCaught(e)
      }

    case _ => super.channelRead(ctx, msg)
  }
}
