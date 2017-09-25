package com.wavesplatform.network.client

import java.net.InetSocketAddress

import com.wavesplatform.network.RawBytes
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor

import scala.concurrent.{Future, Promise}

class NetworkSender(chainId: Char, name: String, nonce: Long) {

  import scala.concurrent.ExecutionContext.Implicits.global

  private val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
  private val client = new NetworkClient(chainId, name, nonce, allChannels)

  def connect(address: InetSocketAddress): Future[Channel] = {
    client.connect(address)
  }

  def send(channel: Channel, messages: RawBytes*): Future[Seq[Unit]] = {
    Future.traverse(messages) { msg =>
      val p = Promise[Unit]
      channel.writeAndFlush(msg).addListener((_: io.netty.util.concurrent.Future[Void]) => p.success(()))
      p.future
    }
  }

  def close(): Unit = client.shutdown()
}
