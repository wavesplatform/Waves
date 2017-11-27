package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import scorex.network.message.{Message => ScorexMessage}
import scorex.utils.ScorexLogging

@Sharable
class TrafficLogger(settings: TrafficLogger.Settings) extends ChannelDuplexHandler with ScorexLogging {

  import BasicMessagesRepo.specsByClasses
  import settings.{ignoreMessages => shouldIgnore}

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = {
    val ignore = msg match {
      case x: RawBytes => shouldIgnore(x.code)
      case x: Message => shouldIgnore(specsByClasses(x.getClass).messageCode)
      case _ => true
    }

    if (!ignore) log.trace(s"${id(ctx)} <-- $msg")
    super.write(ctx, msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
    val ignore = msg match {
      case x: Message => shouldIgnore(specsByClasses(x.getClass).messageCode)
      case _ => true
    }

    if (!ignore) log.trace(s"${id(ctx)} --> $msg")
    super.channelRead(ctx, msg)
  }

}

object TrafficLogger {

  case class Settings(enable: Boolean, ignoreMessages: Set[ScorexMessage.MessageCode])

}