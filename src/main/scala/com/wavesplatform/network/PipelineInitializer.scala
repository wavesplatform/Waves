package com.wavesplatform.network

import com.wavesplatform.network.PipelineInitializer.HandlerWrapper
import io.netty.channel.{Channel, ChannelHandler, ChannelInitializer}
import io.netty.util.concurrent.EventExecutorGroup

class PipelineInitializer[A <: Channel](handlers: => Seq[HandlerWrapper]) extends ChannelInitializer[A] {
  override def initChannel(ch: A) = {
    handlers.foldLeft(ch.pipeline()) {
      case (p, HandlerWrapper(h, None))    => p.addLast(h)
      case (p, HandlerWrapper(h, Some(e))) => p.addLast(e, h)
    }
  }
}

object PipelineInitializer {
  case class HandlerWrapper(h: ChannelHandler, executor: Option[EventExecutorGroup] = None)

  object HandlerWrapper {
    implicit def handlerToWrapper(h: ChannelHandler): HandlerWrapper                      = HandlerWrapper(h)
    implicit def tuple2ToWrapper(h: (ChannelHandler, EventExecutorGroup)): HandlerWrapper = HandlerWrapper(h._1, Some(h._2))
  }
}
