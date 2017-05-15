package com.wavesplatform.network

import java.util.concurrent.ConcurrentHashMap

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelFuture, ChannelHandlerContext, ChannelId, ChannelInboundHandlerAdapter}
import scorex.transaction.History.BlockchainScore
import scorex.utils.ScorexLogging

import scala.concurrent.duration._

@Sharable
class ScoreObserver(scoreTtl: FiniteDuration) extends ChannelInboundHandlerAdapter with ScorexLogging {
  import ScoreObserver._
  val scores = new ConcurrentHashMap[ChannelId, Score]()

  override def handlerAdded(ctx: ChannelHandlerContext) =
    ctx.channel().closeFuture().addListener { f: ChannelFuture =>
      val prevScore = Option(scores.remove(f.channel().id()))
      log.debug(s"Channel ${f.channel().id().asShortText()} closed, removing score${prevScore.fold("")(p => s" (was ${p.value})")}")

    }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case value: BlockchainScore =>
      log.debug(s"Setting score for ${ctx.channel().id().asShortText()} to $value")
      val score = Score(value, System.currentTimeMillis())
      ctx.executor().schedule((() => {
        scores.remove(ctx.channel().id(), score)
      }): Runnable, scoreTtl.length, scoreTtl.unit)
      scores.put(ctx.channel().id(), score)
    case _ => super.channelRead(ctx, msg)
  }
}

object ScoreObserver {
  case class Score(value: BlockchainScore, ts: Long)
}
