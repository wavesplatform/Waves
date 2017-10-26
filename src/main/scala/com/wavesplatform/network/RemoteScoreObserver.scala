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

  private def bestForeignPair: Option[ScorePair] = {
    Option(scores.reduceEntries(1000, (c1, c2) => if (c1.getValue > c2.getValue) c1 else c2))
      .map(e => e.getKey -> e.getValue)
  }

  override def handlerAdded(ctx: ChannelHandlerContext): Unit = {
    ctx.channel().closeFuture().addListener { channelFuture: ChannelFuture =>
      val closedChannel = channelFuture.channel()
      Option(scores.remove(closedChannel)).foreach { removedScore =>
        log.debug(s"${id(ctx)} Closed, removing score $removedScore")
      }

      trySwitchToBestIf(s"switching to second best channel, because ${id(closedChannel)} was closed") {
        case Some((currChannel, _)) => currChannel == closedChannel
        case None => false
      }
    }
  }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = msg match {
    case LocalScoreChanged(newLocalScore) =>
      localScore = newLocalScore
      ctx.writeAndFlush(msg, promise)
      trySwitchToBest("local score was updated because of internal updates")

    case _ => ctx.write(msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case newScore: History.BlockchainScore =>
      ctx.executor().schedule(scoreTtl) {
        if (scores.remove(ctx.channel(), newScore)) trySwitchToBest("score expired")
      }

      val previousScore = scores.put(ctx.channel(), newScore)
      if (previousScore != newScore) log.trace(s"${id(ctx)} New score: $newScore")

      trySwitchToBestIf("new connection")(_.isEmpty)

    case ExtensionBlocks(blocks) if currentRequest.get().exists(_._1 == ctx.channel()) =>
      if (blocks.isEmpty) trySwitchToBest("blockchain is up to date")
      else {
        log.debug(s"${id(ctx)} Receiving extension blocks ${formatBlocks(blocks)}")
        super.channelRead(ctx, msg)
      }

    case ExtensionBlocks(blocks) =>
      log.debug(s"${id(ctx)} Received blocks ${formatBlocks(blocks)} from non-pinned channel")

    case _ => super.channelRead(ctx, msg)
  }

  private def trySwitchToBest(reason: String): Unit = trySwitchToBestIf(reason) { _ => true }

  private def trySwitchToBestIf(reason: String)(cond: Option[ScorePair] => Boolean): Unit = bestForeignPair match {
    case None => currentRequest.set(None)

    case Some(best) =>
      val (bestForeignChannel, bestForeignScore) = best
      val updated: Option[ScorePair] = currentRequest.updateAndGet { (orig: Option[ScorePair]) =>
        if (bestForeignScore <= localScore) None
        else if (cond(orig)) bestForeignPair
        else orig
      }

      if (updated.contains(best)) {
        val toId = Option(bestForeignChannel).map(id(_))
        log.debug(s"A new pinned channel $toId has score $bestForeignScore: $reason, requesting an extension")
        bestForeignChannel.writeAndFlush(LoadBlockchainExtension(lastSignatures))
      } else {
        log.trace(s"Pinned channel was not changed")
      }
  }

}
