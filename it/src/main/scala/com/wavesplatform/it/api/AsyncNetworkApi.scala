package com.wavesplatform.it.api

import java.net.InetSocketAddress

import com.wavesplatform.network.RawBytes
import com.wavesplatform.network.client.NetworkSender

import scala.concurrent.Future

object AsyncNetworkApi {

  implicit class NodeAsyncNetworkApi(n: Node) {

    import scala.concurrent.ExecutionContext.Implicits.global

    def nonce: Long = System.currentTimeMillis()

    def sendByNetwork(message: RawBytes*): Future[Unit] = {
      val sender = new NetworkSender(n.settings.blockchainSettings.addressSchemeCharacter, s"it-test-client-to-${n.name}", nonce)
      sender.connect(new InetSocketAddress(n.nodeInfo.networkIpAddress, n.nodeInfo.hostNetworkPort)).map { ch =>
        if (ch.isActive) sender.send(ch, message: _*).map(_ => sender.close()) else sender.close()
      }
    }
  }
}
