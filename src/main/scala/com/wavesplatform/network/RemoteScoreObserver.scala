package com.wavesplatform.network

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import scorex.transaction.History
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration


@Sharable
class RemoteScoreObserver(scoreTtl: FiniteDuration, lastSignatures: => Seq[ByteStr], initialLocalScore: BigInt)
  extends ChannelDuplexHandler with ScorexLogging {

  private val pinnedChannel = new AtomicReference[Channel]()

  @volatile
  private var localScore = initialLocalScore
  log.debug(s"Initialized with local score $localScore")

  private val scores = new ConcurrentHashMap[Channel, BigInt]

  private def channelWithHighestScore =
    Option(scores.reduceEntries(1000, (c1, c2) => if (c1.getValue > c2.getValue) c1 else c2))
      .map(e => e.getKey -> e.getValue)

  override def handlerAdded(ctx: ChannelHandlerContext) =
    ctx.channel().closeFuture().addListener { f: ChannelFuture =>
      for ((ch, s) <- channelWithHighestScore) {
        // having no channel with highest score means scores map is empty, so it's ok to attempt to remove this channel
        // from the map only when there is one.
        Option(scores.remove(ctx.channel())).foreach(_ => log.debug(s"${id(ctx)} Closed, removing score $s"))
        if (ch == f.channel()) {
          // this channel had the highest score, so we should request extension from second-best channel, just in case
          for ((ch, _) <- channelWithHighestScore) {
            ch.writeAndFlush(LoadBlockchainExtension(lastSignatures))
          }
        }
      }
    }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) = msg match {
    case LocalScoreChanged(newLocalScore) =>
      log.debug(s"Pinned: $pinnedChannelId <- ${id(ctx)}; New local score: $newLocalScore")
      // unconditionally update local score value and propagate this message downstream
      localScore = newLocalScore
      ctx.write(msg, promise)

      // if this is the channel with the highest score and its score is higher than local, request extension
      for ((chan, score) <- channelWithHighestScore if chan == ctx.channel() && score > newLocalScore) {
        chan.writeAndFlush(LoadBlockchainExtension(lastSignatures))
        pinnedChannel.set(chan)
      }

    case _ => ctx.write(msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case newScore: History.BlockchainScore =>
      log.debug(s"Pinned: $pinnedChannelId <- ${id(ctx)}; New score: $newScore")
      ctx.executor().schedule(scoreTtl) {
        if (scores.remove(ctx.channel(), newScore)) {
          log.debug(s"${id(ctx)} Score expired, removing $newScore")
        }
      }

      val previousScore = scores.put(ctx.channel(), newScore)

      for {
        (ch, highScore) <- channelWithHighestScore
        if ch == ctx.channel() && // this is the channel with highest score
          (previousScore == null || previousScore < newScore) && // score has increased
          highScore > localScore && // remote score is higher than local
          pinnedChannel.compareAndSet(null, ch) // and we've finished to download blocks from previous high score channel
      } {
        log.debug(s"${id(ctx)} New high score $highScore > $localScore, requesting extension")
        ctx.writeAndFlush(LoadBlockchainExtension(lastSignatures))
      }

    case _: ExtensionBlocks if pinnedChannel.compareAndSet(ctx.channel(), null) =>
      log.debug(s"Pinned: $pinnedChannelId <- ${id(ctx)}: Receiving extension blocks")
      super.channelRead(ctx, msg)

    case ExtensionBlocks(blocks) =>
      log.debug(s"${id(ctx)} Unexpected extension blocks: ${blocks.head.uniqueId} .. ${blocks.last.uniqueId} (Pinned: $pinnedChannelId)")
      super.channelRead(ctx, msg)

    case _ => super.channelRead(ctx, msg)
  }

  private def pinnedChannelId = Option(pinnedChannel.get()).fold("[NONE]"){id}
}
