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

  private def bestRemotePair: Option[ScorePair] = {
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
        case _ => false
      }
    }
  }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = msg match {
    case LocalScoreChanged(newLocalScore) =>
      localScore = newLocalScore
      ctx.writeAndFlush(msg, promise)
      trySwitchToBestIf("local score was updated because of internal updates")(_.isEmpty)

    case _ => ctx.write(msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case newScore: History.BlockchainScore =>
      val previousScore = Option(scores.put(ctx.channel(), newScore)).getOrElse(BigInt(0))
      if (previousScore != newScore) {
        scheduleExpiration(ctx, newScore)
        log.trace(s"${id(ctx)} New score: $newScore (diff: ${newScore - previousScore})")
        trySwitchToBestIf("new connection")(_.isEmpty)
      }

    case ExtensionBlocks(blocks) =>
      val isExpectedResponse = currentRequest.get().exists(_._1 == ctx.channel())
      if (!isExpectedResponse) {
        log.debug(s"${id(ctx)} Received blocks ${formatBlocks(blocks)} from non-pinned channel (could be from expired channel)")
      } else if (blocks.isEmpty) {
        trySwitchToBest("blockchain is up to date")
      } else {
        log.debug(s"${id(ctx)} Receiving extension blocks ${formatBlocks(blocks)}")
        super.channelRead(ctx, msg)
      }

    case _ => super.channelRead(ctx, msg)
  }

  private def scheduleExpiration(ctx: ChannelHandlerContext, score: BigInt): Unit = {
    ctx.executor().schedule(scoreTtl) {
      if (scores.remove(ctx.channel(), score)) trySwitchToBestIf("score expired") {
        case Some((channel, _)) => channel == ctx.channel()
        case _ => false
      }
    }
  }

  private def trySwitchToBest(reason: String): Unit = trySwitchToBestIf(reason) { _ => true }

  private def trySwitchToBestIf(reason: String)(cond: Option[ScorePair] => Boolean): Unit = bestRemotePair match {
    case None => currentRequest.set(None)
    case Some(best@(bestRemoteChannel, bestRemoteScore)) =>
      currentRequest
        .updateAndGet { (orig: Option[ScorePair]) =>
          if (bestRemoteScore <= localScore) None
          else if (cond(orig)) bestRemotePair
          else orig
        }
        .filter(_ == best)
        .foreach { _ =>
          val toId = Option(bestRemoteChannel).map(id(_))
          log.debug(
            s"A new pinned channel $toId has score $bestRemoteScore (diff with local: ${bestRemoteScore - localScore}): " +
              s"$reason, requesting an extension"
          )
          bestRemoteChannel.writeAndFlush(LoadBlockchainExtension(lastSignatures))
        }
  }

}
