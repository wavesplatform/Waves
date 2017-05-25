package com.wavesplatform.network

import io.netty.channel.Channel

trait Network {
  def requestExtension(localScore: BigInt): Unit
  def broadcast(msg: AnyRef, except: Option[Channel] = None): Unit
}
