package com.wavesplatform.network

import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import scorex.transaction.History
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration


@Sharable
class RemoteScoreObserver(scoreTtl: FiniteDuration, lastSignatures: => Seq[ByteStr])
  extends ChannelDuplexHandler with ScorexLogging {
  import RemoteScoreObserver._

  @volatile
  private var localScore = BigInt(0)
  private val scores = new ConcurrentHashMap[Channel, RemoteScore]

  private def channelWithHighestScore =
    Option(scores.reduceEntries(1000, (c1, c2) => if (c1.getValue.value > c2.getValue.value) c1 else c2))
      .map(e => e.getKey -> e.getValue)

  override def handlerAdded(ctx: ChannelHandlerContext) =
    ctx.channel().closeFuture().addListener { f: ChannelFuture =>
      val previousChannel = channelWithHighestScore.fold(f.channel())(_._1)
      Option(scores.remove(f.channel())).foreach { s =>
        log.debug(s"${id(ctx)} Closed, removing score ${s.value}")
      }

      // if the channel which is being removed had the highest score, load extension from the best among remaining ones
      channelWithHighestScore.filter(_._1 != previousChannel).foreach {
        case (ch, _) => ch.writeAndFlush(LoadBlockchainExtension(lastSignatures))
      }
    }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) = msg match {
    case LocalScoreChanged(newLocalScore) =>
      localScore = newLocalScore
      channelWithHighestScore match {
        case Some((chan, score)) =>
          promise.setSuccess()
          if (score.value > newLocalScore) {
            log.debug(s"${id(ctx)} Local score $newLocalScore is still lower than remote ${score.value}, requesting extension")
            chan.writeAndFlush(LoadBlockchainExtension(lastSignatures))
          } else {
            log.trace(s"${id(ctx)} Blockchain is up to date")
          }
        case _ => log.debug(s"${id(ctx)} No channels left?")
      }
    case _ => ctx.write(msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case newScoreValue: History.BlockchainScore =>
      val score = RemoteScore(newScoreValue, System.currentTimeMillis())
      ctx.executor().schedule(scoreTtl) {
        scores.remove(ctx.channel().id(), score)
      }

      val previousChannelScore = scores.put(ctx.channel(), score)

      channelWithHighestScore match {
        case Some((ch, highScore)) if ch == ctx.channel() /* this is the channel with highest score */
          && (previousChannelScore == null || previousChannelScore.value != newScoreValue) /* score has changed */
          && highScore.value > localScore /* remote score is higher than local */ =>
          log.debug(s"${id(ctx)} New high score ${highScore.value} > $localScore, requesting extension")
          ctx.writeAndFlush(LoadBlockchainExtension(lastSignatures))
        case _ =>
      }
    case _ => super.channelRead(ctx, msg)
  }
}

object RemoteScoreObserver {
  private[RemoteScoreObserver] case class RemoteScore(value: History.BlockchainScore, ts: Long)
}
