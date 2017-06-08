package com.wavesplatform.it

import java.net.InetSocketAddress
import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.it.network.{NetworkServer, PeerInfo}
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import org.scalatest.FreeSpec

class NetworkTestSuite(allNodes: Seq[Node]) extends FreeSpec {
  val establishedConnections = new ConcurrentHashMap[Channel, PeerInfo]
  val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
  val f = allNodes.head
  val s = new NetworkServer('T', f.settings, allChannels, establishedConnections)
  s.connect(new InetSocketAddress("localhost", f.nodeInfo.hostNetworkPort))
//  s.writeToLocalChannel()
  Thread.sleep(10000)
  assert(establishedConnections.size() == 1)
}
