package com.wavesplatform.network

import com.typesafe.scalalogging.Logger
import com.wavesplatform.network.message.Message as ScorexMessage
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import pureconfig.*

abstract class TrafficLogger(settings: TrafficLogger.Settings) extends ChannelDuplexHandler {
  protected def codeOf(msg: AnyRef): Option[Byte]
  protected def stringify(msg: Any): String
  protected def logger: Logger

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = {
    codeOf(msg).filterNot(settings.ignoreTxMessages).foreach { code =>
      logger.trace(s"${id(ctx)} <-- transmitted($code): ${stringify(msg)}")
    }

    super.write(ctx, msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
    codeOf(msg).filterNot(settings.ignoreRxMessages).foreach { code =>
      logger.trace(s"${id(ctx)} --> received($code): ${stringify(msg)}")
    }

    super.channelRead(ctx, msg)
  }
}

object TrafficLogger {

  case class Settings(ignoreTxMessages: Set[ScorexMessage.MessageCode], ignoreRxMessages: Set[ScorexMessage.MessageCode]) derives ConfigReader

}
