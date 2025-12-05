package com.wavesplatform.finalization

import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup

import scala.util.Using
import scala.util.Using.Releasable

trait WithManager {
  given Releasable[DefaultChannelGroup]           = _.close()
  given Releasable[EmbeddedChannel]               = _.close()
  def withManager(f: Using.Manager => Unit): Unit = Using.Manager(f).get
}
