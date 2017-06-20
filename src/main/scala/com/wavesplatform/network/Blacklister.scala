package com.wavesplatform.network

import io.netty.channel.Channel

trait Blacklister {
  def blacklist(channel: Channel): Unit
}
