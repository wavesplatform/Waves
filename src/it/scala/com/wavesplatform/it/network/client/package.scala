package com.wavesplatform.it.network

import io.netty.channel.Channel

package object client {
  def id(chan: Channel): String = s"[${chan.id().asShortText()}]"
}
