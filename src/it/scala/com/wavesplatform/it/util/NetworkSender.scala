package com.wavesplatform.it.util

import java.net.InetSocketAddress
import java.util.concurrent.{ConcurrentHashMap, ThreadFactory}

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.it.network.client.{NetworkClient, PeerInfo, RawBytes}
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.{HashedWheelTimer, Timer}
import io.netty.util.concurrent.GlobalEventExecutor

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global

class NetworkSender(address: InetSocketAddress, chainId: Char, name: String, nonce: Long) {
  private val retryTimer = new HashedWheelTimer(new ThreadFactoryBuilder().setDaemon(true).build())
  retryTimer.start()
  def sendByNetwork(messages: RawBytes*): Future[Unit] = {
    val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    val establishedConnections = new ConcurrentHashMap[Channel, PeerInfo]
    val c = new NetworkClient(chainId, name, nonce, allChannels, establishedConnections)
    c.connect(address)
    retryTimer.retryUntil(Future.successful(establishedConnections.size()), (size: Int) => size == 1, 1.seconds)
      .map(_ => {
      val channel = establishedConnections.asScala.head._1
      messages.foreach(channel.writeAndFlush)
      c.shutdown()
    })
  }
}
