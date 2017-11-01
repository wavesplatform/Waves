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

  private type ScorePair = (Channel, BigInt)

  private val scores = new ConcurrentHashMap[Channel, BigInt]

  @volatile private var localScore = initialLocalScore
  private val currentRequest = new AtomicReference[Option[ScorePair]](None)

  private def channelWithHighestScore: Option[ScorePair] = {
    Option(scores.reduceEntries(1000, (c1, c2) => if (c1.getValue > c2.getValue) c1 else c2))
      .map(e => e.getKey -> e.getValue)
  }

  override def handlerAdded(ctx: ChannelHandlerContext): Unit = {
    ctx.channel().closeFuture().addListener { channelFuture: ChannelFuture =>
      val closedChannel = channelFuture.channel()
      Option(scores.remove(closedChannel)).foreach { removedScore =>
        log.debug(s"${id(ctx)} Closed, removing score $removedScore")
      }

      trySwitchToBestIf(ctx, s"Switching to second best channel, because ${id(closedChannel)} was closed")(_.contains(closedChannel))
    }
  }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = msg match {
    case LocalScoreChanged(newLocalScore) =>
      localScore = newLocalScore
      ctx.writeAndFlush(msg, promise)
      trySwitchToBestIf(ctx, "A local score was updated because of internal updates")(_.isEmpty)

    case _ => ctx.write(msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case newScore: History.BlockchainScore =>
      val diff = newScore - Option(scores.put(ctx.channel(), newScore)).getOrElse(BigInt(0))
      if (diff != 0) {
        scheduleExpiration(ctx, newScore)
        log.trace(s"${id(ctx)} New score: $newScore (diff: $diff)")
        trySwitchToBestIf(ctx, "A new score")(_.isEmpty)
      }

    case ExtensionBlocks(blocks) =>
      val isExpectedResponse = currentRequest.get().exists(_._1 == ctx.channel())
      if (!isExpectedResponse) {
        log.debug(s"${id(ctx)} Received blocks ${formatBlocks(blocks)} from non-pinned channel (could be from expired channel)")
      } else if (blocks.isEmpty) {
        trySwitchToBestIf(ctx, "Blockchain is up to date with requested extension") { _ => true }
      } else {
        log.debug(s"${id(ctx)} Receiving extension blocks ${formatBlocks(blocks)}")
        super.channelRead(ctx, msg)
      }

    case _ => super.channelRead(ctx, msg)
  }

  private def scheduleExpiration(ctx: ChannelHandlerContext, score: BigInt): Unit = {
    ctx.executor().schedule(scoreTtl) {
      if (scores.remove(ctx.channel(), score)) trySwitchToBestIf(ctx, "score expired")(_.contains(ctx.channel()))
    }
  }

  private def trySwitchToBestIf(initiatorCtx: ChannelHandlerContext, reason: String)
                               (cond: Option[Channel] => Boolean): Unit = channelWithHighestScore match {
    case None => currentRequest.set(None)
    case best@Some((bestRemoteChannel, bestRemoteScore)) =>
      currentRequest
        .updateAndGet { (orig: Option[ScorePair]) =>
          if (bestRemoteScore <= localScore) None
          else if (cond(orig.map(_._1))) best
          else orig
        }
        .filter(best.contains)
        .foreach { _ =>
          log.debug(
            s"${id(initiatorCtx)} A new pinned channel ${id(bestRemoteChannel)} has score $bestRemoteScore " +
              s"(diff with local: ${bestRemoteScore - localScore}): requesting an extension. $reason"
          )
          bestRemoteChannel.writeAndFlush(LoadBlockchainExtension(lastSignatures))
        }
  }

}
