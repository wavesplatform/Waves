package com.wavesplatform.network

import com.wavesplatform.metrics.Metrics
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import org.influxdb.dto.Point
import scorex.network.message.MessageSpec

@Sharable
class TrafficWatcher extends ChannelDuplexHandler {

  import BasicMessagesRepo.specsByCodes

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = {
    msg match {
      case x: RawBytes => specsByCodes.get(x.code).foreach(measure(_, x, "outgoing"))
      case _ =>
    }

    super.write(ctx, msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
    msg match {
      case x: RawBytes => specsByCodes.get(x.code).foreach(measure(_, x, "incoming"))
      case _ =>
    }

    super.channelRead(ctx, msg)
  }

  protected def measure(spec: MessageSpec[_], msg: RawBytes, dir: String): Unit = {
    Metrics.write(Point.measurement("traffic")
      .tag("dir", dir)
      .tag("type", spec.messageName)
      .addField("type", spec.messageName)
      .addField("bytes", msg.data.length))
  }

}
