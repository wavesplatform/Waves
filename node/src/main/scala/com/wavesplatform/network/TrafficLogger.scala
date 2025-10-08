package com.wavesplatform.network

import com.wavesplatform.network.message.Message as ScorexMessage
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import pureconfig.*

abstract class TrafficLogger(settings: TrafficLogger.Settings) extends ChannelDuplexHandler with ScorexLogging {
  protected def codeOf(msg: AnyRef): Option[Byte]
  protected def stringify(msg: Any): String

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = {
    codeOf(msg).filterNot(settings.ignoreTxMessages).foreach { code =>
      log.trace(s"${id(ctx)} <-- transmitted($code): ${stringify(msg)}")
    }

    super.write(ctx, msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
    codeOf(msg).filterNot(settings.ignoreRxMessages).foreach { code =>
      log.trace(s"${id(ctx)} --> received($code): ${stringify(msg)}")
    }

    super.channelRead(ctx, msg)
  }
}

object TrafficLogger {

  case class Settings(ignoreTxMessages: Set[ScorexMessage.MessageCode], ignoreRxMessages: Set[ScorexMessage.MessageCode]) derives ConfigReader

}
