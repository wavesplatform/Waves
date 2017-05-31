package com.wavesplatform.network

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import com.wavesplatform.utils.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import scorex.transaction.History
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration


@Sharable
class RemoteScoreObserver(scoreTtl: FiniteDuration, lastSignatures: => Seq[ByteStr])
  extends ChannelDuplexHandler with ScorexLogging {
  import RemoteScoreObserver._

  private val scores = new ConcurrentHashMap[Channel, RemoteScore]
  private val pinnedChannel = new AtomicReference[Option[Channel]](None)

  private def channelWithHighestScore =
    Option(scores.reduceEntries(1000, (c1, c2) => if (c1.getValue.value > c2.getValue.value) c1 else c2))
      .map(e => e.getKey -> e.getValue)

  override def handlerAdded(ctx: ChannelHandlerContext) =
    ctx.channel().closeFuture().addListener { f: ChannelFuture =>
      val ch = f.channel()
      val prevScore = Option(scores.remove(ch))
      pinnedChannel.compareAndSet(Some(ch), channelWithHighestScore.map(_._1))
      log.debug(s"${id(ctx)} Closed, removing score${prevScore.fold("")(p => s" (was ${p.value})")}")
    }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) = msg match {
    case LocalScoreChanged(newLocalScore) =>
      channelWithHighestScore match {
        case Some((chan, score)) =>
          promise.setSuccess()
          if (score.value > newLocalScore) {
            log.debug(s"${id(ctx)} Local score $newLocalScore is still lower than remote ${score.value} from ${id(chan)}, requesting extension")
            chan.writeAndFlush(LoadBlockchainExtension(lastSignatures))
          } else {
            log.debug(s"${id(ctx)} Blockchain is up to date")
          }
        case _ => log.debug(s"${id(ctx)} No channel with high score")
      }
    case _ => ctx.write(msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case newScoreValue: History.BlockchainScore =>
      val isNewHighScore = channelWithHighestScore.forall(_._2.value < newScoreValue)
      val score = RemoteScore(newScoreValue, System.currentTimeMillis())
      scores.compute(ctx.channel(), { (_, prevScore) =>
        if (prevScore == null || score.value > prevScore.value && score.ts >= prevScore.ts) {
          log.debug(s"${id(ctx)} New score: $newScoreValue${if (isNewHighScore) " (new high score)" else ""}")
        }
        score
      })

      ctx.executor().schedule(scoreTtl) {
        scores.remove(ctx.channel().id(), score)
      }

      if (isNewHighScore && pinnedChannel.getAndSet(Some(ctx.channel())).isEmpty) {
        log.debug(s"${id(ctx)} No previously pinned channel, requesting signatures")
        ctx.write(LoadBlockchainExtension(lastSignatures))
      }

      scores.put(ctx.channel(), score)
    case _ => super.channelRead(ctx, msg)
  }
}

object RemoteScoreObserver {
  private[RemoteScoreObserver] case class RemoteScore(value: History.BlockchainScore, ts: Long)
}
