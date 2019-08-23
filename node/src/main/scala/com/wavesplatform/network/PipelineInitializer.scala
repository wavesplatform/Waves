package com.wavesplatform.network

import io.netty.channel.{Channel, ChannelHandler, ChannelInitializer}

class PipelineInitializer(handlers: => Seq[ChannelHandler]) extends ChannelInitializer[Channel] {
  override def initChannel(ch: Channel): Unit = {
    handlers.foldLeft(ch.pipeline())(_.addLast(_))
  }
}
