package com.wavesplatform.network

import io.netty.channel.Channel

trait Broadcaster {
  def broadcast(msg: AnyRef, except: Option[Channel]): Unit
}
