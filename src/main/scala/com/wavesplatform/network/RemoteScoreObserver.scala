package com.wavesplatform.network

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import scorex.transaction.History
import scorex.utils.ScorexLogging

import scala.concurrent.{Future, blocking}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

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

  override def handlerAdded(ctx: ChannelHandlerContext): Unit =
    ctx.channel().closeFuture().addListener { f: ChannelFuture =>
      for ((bestChannel, _) <- channelWithHighestScore) {
        // having no channel with highest score means scores map is empty, so it's ok to attempt to remove this channel
        // from the map only when there is one.
        Option(scores.remove(ctx.channel())).foreach(removedScore => log.debug(s"${id(ctx)} Closed, removing score $removedScore"))
        if (bestChannel == f.channel()) {
          // this channel had the highest score, so we should request extension from second-best channel, just in case
          channelWithHighestScore match {
            case Some((secondBestChannel, secondBestScore))
              if secondBestScore > localScore && pinnedChannel.compareAndSet(bestChannel, secondBestChannel) =>
              log.debug(s"${id(ctx)} Switching to second best channel $pinnedChannelId")
              requestExtension(secondBestChannel)
            case _ =>
              if (pinnedChannel.compareAndSet(f.channel(), null)) log.trace(s"${id(ctx)} Unpinning unconditionally")
          }
        } else {
          if (pinnedChannel.compareAndSet(ctx.channel(), null))
            log.debug(s"${id(ctx)} ${pinnedChannelId}Closing channel and unpinning")
        }
      }
    }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = msg match {
    case LocalScoreChanged(newLocalScore, breakExtProcessing) =>
      if (localScore != newLocalScore) log.debug(s"${id(ctx)} $pinnedChannelId New local score: $newLocalScore")
      localScore = newLocalScore
      ctx.write(msg, promise)

      if (breakExtProcessing && pinnedChannel.compareAndSet(ctx.channel(), null)) {
        log.debug(s"${id(ctx)} Stop processing an extension")
        channelWithHighestScore match {
          case Some((bestChannel, bestScore))
            if bestScore > localScore && pinnedChannel.compareAndSet(null, bestChannel) =>
            if (ctx.channel() != bestChannel) {
              log.debug(s"${id(ctx)} Stopping an optimistically loading of extension, because the channel ${id(ctx.channel())} will be outdated")
              ctx.write(LoadBlockchainExtension(Seq.empty))
            }

            log.debug(s"${id(ctx)} Switching to next best channel ${id(bestChannel)}")
            requestExtension(bestChannel)
          case _ =>
            ctx.write(LoadBlockchainExtension(Seq.empty))
        }
      }

    case _ => ctx.write(msg, promise)
  }

  private def requestExtension(channel: Channel): Unit = Future(blocking(lastSignatures)).onComplete {
    case Success(sig) => channel.writeAndFlush(LoadBlockchainExtension(sig))
    case Failure(e) => log.warn("Error getting last signatures", e)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
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
        log.debug(s"${id(ctx)} ${pinnedChannelId}New high score $highScore > $localScore, pinning and requesting extension")
        requestExtension(ch)
      } else {
        log.trace(s"${id(ctx)} New high score $highScore, but we are processing a score from ${pinnedChannelId}for now")
      }

    case ExtensionBlocks(blocks) if pinnedChannel.get() == ctx.channel() =>
      if (blocks.nonEmpty) {
        log.debug(s"${id(ctx)} ${pinnedChannelId}Receiving extension blocks ${formatBlocks(blocks)}")
        super.channelRead(ctx, msg)
      } else {
        log.debug(s"${id(ctx)} ${pinnedChannelId}Blockchain is up to date")
        if (pinnedChannel.compareAndSet(ctx.channel(), null)) log.debug(s"${id(ctx)} Successfully reset pinned channel")
        else log.debug(s"${id(ctx)} The pinned channel is unchanged, $pinnedChannelId")
      }

    case ExtensionBlocks(blocks) =>
      log.debug(s"${id(ctx)} ${pinnedChannelId}Received blocks ${formatBlocks(blocks)} from non-pinned channel")

    case _ => super.channelRead(ctx, msg)
  }

  private def pinnedChannelId = Option(pinnedChannel.get()).fold("")(ch => s"${id(ch, "pinned: ")} ")
}
