package com.wavesplatform.network

import com.wavesplatform.network.message.{Message => ScorexMessage}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import kamon.Kamon
import kamon.metric.{Histogram, MeasurementUnit}

@Sharable
class TrafficWatcher extends ChannelDuplexHandler {

  import BasicMessagesRepo.specsByCodes

  private val outgoing: Map[ScorexMessage.MessageCode, Histogram] = specsByCodes.map { case (code, spec) =>
    code -> createHistogram("outgoing", spec)
  }

  private val incoming: Map[ScorexMessage.MessageCode, Histogram] = specsByCodes.map { case (code, spec) =>
    code -> createHistogram("incoming", spec)
  }

  private def createHistogram(dir: String, spec: BasicMessagesRepo.Spec): Histogram =
    Kamon
      .histogram("traffic", MeasurementUnit.information.bytes)
      .withTag("type", spec.messageName)
      .withTag("dir", dir)

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = {
    msg match {
      case x: RawBytes => outgoing.get(x.code).foreach(_.record(x.data.length))
      case _           =>
    }

    super.write(ctx, msg, promise)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
    msg match {
      case x: RawBytes => incoming.get(x.code).foreach(_.record(x.data.length))
      case _           =>
    }

    super.channelRead(ctx, msg)
  }

}
