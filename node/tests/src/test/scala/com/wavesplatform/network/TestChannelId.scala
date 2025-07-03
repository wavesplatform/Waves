package com.wavesplatform.network

import io.netty.channel.ChannelId

case class TestChannelId(label: String) extends ChannelId {
  override def asShortText(): String = label

  override def asLongText(): String = label

  override def compareTo(o: ChannelId): Int = o match {
    case o: TestChannelId => label.compareTo(o.label)
    case _                => -1
  }
}
