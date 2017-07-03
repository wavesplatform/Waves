package com.wavesplatform.it.network

import com.wavesplatform.network.AttributeKeys
import io.netty.channel.{Channel, ChannelHandlerContext}

package object client {
  def id(chan: Channel): String = s"[${chan.id().asShortText()}: ${chan.attr(AttributeKeys.NodeName)}@${chan.attr(AttributeKeys.DeclaredAddress)}]"
}
