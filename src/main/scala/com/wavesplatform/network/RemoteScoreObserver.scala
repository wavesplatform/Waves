package com.wavesplatform.network

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import com.wavesplatform.settings.SynchronizationSettings
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import scorex.transaction.History
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._


@Sharable
class RemoteScoreObserver(syncSettings: SynchronizationSettings)
  extends ChannelDuplexHandler with ScorexLogging {
  import RemoteScoreObserver._

  private val scores = new ConcurrentHashMap[Channel, RemoteScore]
  private val pinnedChannel = new AtomicReference[Option[Channel]](None)

  private def channelWithHighestScore = if (scores.isEmpty) None else Some(scores.asScala.maxBy(_._2.value))

  override def handlerAdded(ctx: ChannelHandlerContext) =
    ctx.channel().closeFuture().addListener { f: ChannelFuture =>
      val ch = f.channel()
      val prevScore = Option(scores.remove(ch))
      val newPinnedChannel = channelWithHighestScore.map(_._1)
      pinnedChannel.compareAndSet(Some(ch), newPinnedChannel)
      log.debug(s"Channel ${ch.id().asShortText()} closed, removing score${prevScore.fold("")(p => s" (was ${p.value})")}")
    }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise) = msg match {
    case LocalScoreChanged(newLocalScore) =>
      for ((chan, score) <- channelWithHighestScore if chan == ctx.channel() && score.value > newLocalScore) {
        log.debug(s"Local score $newLocalScore is still lower than remote score ${score.value} from ${ctx.channel().id().asShortText()}, requesting extension")
        ctx.write(ExtensionSignaturesLoader.LoadSignatures, promise)
      }
    case _ => ctx.write(msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case newScoreValue: History.BlockchainScore =>
      log.debug(s"Setting score for ${ctx.channel().id().asShortText()} to $newScoreValue")
      val score = RemoteScore(newScoreValue, System.currentTimeMillis())

      ctx.executor().schedule(syncSettings.scoreTTL) {
        scores.remove(ctx.channel().id(), score)
      }

      if (channelWithHighestScore.forall(_._2.value < newScoreValue)) {
        pinnedChannel.set(Some(ctx.channel()))
        log.debug(s"We have a new highest score $newScoreValue")
        ctx.write(ExtensionSignaturesLoader.LoadSignatures)
      }

      scores.put(ctx.channel(), score)
    case _ => super.channelRead(ctx, msg)
  }
}

object RemoteScoreObserver {
  case class RemoteScore(value: History.BlockchainScore, ts: Long)
}
