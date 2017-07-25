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

  private val scores = new ConcurrentHashMap[Channel, BigInt]

  private def channelWithHighestScore =
    Option(scores.reduceEntries(1000, (c1, c2) => if (c1.getValue > c2.getValue) c1 else c2))
      .map(e => e.getKey -> e.getValue)

  override def handlerAdded(ctx: ChannelHandlerContext) =
    ctx.channel().closeFuture().addListener { f: ChannelFuture =>
      for ((currentHighestScoredChannel, s) <- channelWithHighestScore) {
        // having no channel with highest score means scores map is empty, so it's ok to attempt to remove this channel
        // from the map only when there is one.
        Option(scores.remove(ctx.channel())).foreach(_ => log.debug(s"${id(ctx)} Closed, removing score $s"))
        if (currentHighestScoredChannel == f.channel()) {
          // this channel had the highest score, so we should request extension from second-best channel, just in case
          for ((secondHighestScoredChannel, _) <- channelWithHighestScore if pinnedChannel.compareAndSet(currentHighestScoredChannel, secondHighestScoredChannel)) {
            secondHighestScoredChannel.writeAndFlush(LoadBlockchainExtension(lastSignatures))
          }
        } else {
          if (pinnedChannel.compareAndSet(ctx.channel(), null))
            log.debug(s"${id(ctx)} ${pinnedChannelId}Closing channel and unpinning")
        }
      }
    }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) = msg match {
    case LocalScoreChanged(newLocalScore) =>
      if (pinnedChannel.compareAndSet(ctx.channel(), null)) { // Fork applied
        log.debug(s"${id(ctx)} ${pinnedChannelId}New local score: $newLocalScore")
      }
      // unconditionally update local score value and propagate this message downstream
      localScore = newLocalScore
      ctx.write(msg, promise)

      // if this is the channel with the highest score and its score is higher than local, request extension
      for ((chan, score) <- channelWithHighestScore if chan == ctx.channel() && score > newLocalScore) {
        log.debug(s"${id(ctx)} ${pinnedChannelId}Pinning this channel")
        pinnedChannel.set(chan)
        chan.writeAndFlush(LoadBlockchainExtension(lastSignatures))
      }

    case _ => ctx.write(msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case newScore: History.BlockchainScore =>

      ctx.executor().schedule(scoreTtl) {
        if (scores.remove(ctx.channel(), newScore)) {
          log.trace(s"${id(ctx)} Score expired, removing $newScore")
        }
      }

      val previousScore = scores.put(ctx.channel(), newScore)
      if (previousScore != newScore) {
        log.trace(s"${id(ctx)} ${pinnedChannelId}New score: $newScore")
      }

      for {
        (ch, highScore) <- channelWithHighestScore
        if ch == ctx.channel() && // this is the channel with highest score
          (previousScore == null || previousScore < newScore) && // score has increased
          highScore > localScore // remote score is higher than local
      } if (pinnedChannel.compareAndSet(null, ch)) {
        // we've finished to download blocks from previous high score channel
        log.debug(s"${id(ctx)} New high score $highScore > $localScore, requesting extension")
        ctx.writeAndFlush(LoadBlockchainExtension(lastSignatures))
      } else {
        log.trace(s"${id(ctx)} New high score $highScore")
      }

    case ExtensionBlocks(blocks) if pinnedChannel.get() == ctx.channel() =>
      if (blocks.nonEmpty){
        log.debug(s"${id(ctx)} ${pinnedChannelId}Receiving extension blocks ${formatBlocks(blocks)}")
        super.channelRead(ctx, msg)
      } else {
        log.debug(s"${id(ctx)} ${pinnedChannelId}Blockchain is up to date")
        pinnedChannel.compareAndSet(ctx.channel(), null)
      }

    case ExtensionBlocks(blocks) =>
      log.debug(s"${id(ctx)} ${pinnedChannelId}Received blocks ${formatBlocks(blocks)} from non-pinned channel")
      super.channelRead(ctx, msg)

    case _ => super.channelRead(ctx, msg)
  }

  private def pinnedChannelId = Option(pinnedChannel.get()).fold("")(ch => s"${id(ch, "pinned: ")} ")
}
