package com.wavesplatform.it

import java.util.concurrent.CancellationException

import io.netty.channel.{Channel, ChannelFuture}

import scala.concurrent.{Future, Promise}
import scala.util.Try

package object network {
  implicit class NettyScalaFuture(nettyFuture: ChannelFuture) {
    def asScala: Future[Channel] = {
      val p = Promise[Channel]()
      nettyFuture.addListener(
        (future: ChannelFuture) =>
          p complete Try(
            if (future.isSuccess) future.channel()
            else if (future.isCancelled) throw new CancellationException
            else throw future.cause()))
      p.future
    }
  }
}
