package com.wavesplatform.network

import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.settings.SynchronizationSettings
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import scorex.network.Handshake
import scorex.transaction.History
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._


@Sharable
class ScoreObserver(syncSettings: SynchronizationSettings, history: History)
  extends ChannelInboundHandlerAdapter with ScorexLogging {
  import ScoreObserver._

  val scores = new ConcurrentHashMap[Channel, Score]()

  def maxScore = scores.asScala.foldLeft(BigInt(0)) {
    case (prev, (_, s)) => prev.max(s.value)
  }

  override def handlerAdded(ctx: ChannelHandlerContext) =
    ctx.channel().closeFuture().addListener { f: ChannelFuture =>
      val prevScore = Option(scores.remove(f.channel().id()))
      log.debug(s"Channel ${f.channel().id().asShortText()} closed, removing score${prevScore.fold("")(p => s" (was ${p.value})")}")

    }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case _: Handshake =>
      log.debug(s"New channel opened, broadcasting local score ${history.score()}")
      ctx.write(history.score())
    case newScoreValue: History.BlockchainScore =>
      log.debug(s"Setting score for ${ctx.channel().id().asShortText()} to $newScoreValue")
      val score = Score(newScoreValue, System.currentTimeMillis())

      ctx.executor().schedule(syncSettings.scoreTTL) {
        scores.remove(ctx.channel().id(), score)
      }

      if (newScoreValue > maxScore) {
        log.debug(s"We have a new highest score $newScoreValue")
        ctx.fireChannelRead(NewHighScoreReceived)
      }

      scores.put(ctx.channel(), score)
    case _ => super.channelRead(ctx, msg)
  }
}

object ScoreObserver {
  case class Score(value: History.BlockchainScore, ts: Long)
  case object NewHighScoreReceived
}
