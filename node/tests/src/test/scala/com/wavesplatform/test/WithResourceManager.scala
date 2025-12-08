package com.wavesplatform.test

import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup

import scala.util.Using
import scala.util.Using.Releasable

trait WithResourceManager {
  given Releasable[DefaultChannelGroup]           = _.close()
  given Releasable[EmbeddedChannel]               = _.close()
  def withManager(f: Using.Manager => Unit): Unit = Using.Manager(f).get
}
