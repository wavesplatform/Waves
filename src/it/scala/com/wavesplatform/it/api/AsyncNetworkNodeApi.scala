package com.wavesplatform.it.api

import java.net.InetSocketAddress

import com.wavesplatform.network.client.NetworkSender
import com.wavesplatform.network.RawBytes

import scala.concurrent.Future

trait AsyncNetworkNodeApi { this : Node =>

  import scala.concurrent.ExecutionContext.Implicits.global

  def nonce: Long = System.currentTimeMillis()

  def sendByNetwork(message: RawBytes*): Future[Unit] = {
    val sender = new NetworkSender(chainId, nodeName, nonce)
    sender.connect(new InetSocketAddress(restAddress, networkPort)).map { ch =>
      if (ch.isActive) sender.send(ch, message: _*).map(_ => sender.close()) else sender.close()
    }
  }
}
