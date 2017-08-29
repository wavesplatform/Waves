package com.wavesplatform.network


import java.util.concurrent.TimeUnit

import com.google.common.cache.{Cache, CacheBuilder}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext}
import scorex.transaction.History
import scorex.utils.{ScorexLogging, Time}

import scala.concurrent.duration._

@Sharable
class DiscardingHandler(history: History, time: Time, maxBlockchainAge: Duration, blockchainReadinessCacheTime: Duration) extends ChannelDuplexHandler with ScorexLogging {

  import DiscardingHandler._

  private val readinessCache: Cache[Object, Boxed] = CacheBuilder
    .newBuilder()
    .expireAfterWrite(blockchainReadinessCacheTime.toMillis, TimeUnit.MILLISECONDS)
    .build[Object, Boxed]()

  private def blockchainReady: Boolean = readinessCache.get(key, () => Boxed(time.correctedTime() - history.lastBlock.get.timestamp < maxBlockchainAge.toMillis)).value

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case RawBytes(code, _) if code == TransactionMessageSpec.messageCode && blockchainReady =>
      log.trace(s"${id(ctx)} Discarding incoming message $code")
    case _ => super.channelRead(ctx, msg)
  }
}

object DiscardingHandler {

  case class Boxed(value: Boolean)
  private val key = new Object

}
