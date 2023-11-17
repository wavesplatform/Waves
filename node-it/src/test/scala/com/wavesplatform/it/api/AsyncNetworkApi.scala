package com.wavesplatform.it.api

import com.wavesplatform.it.Node
import com.wavesplatform.network.client.NetworkSender

import java.net.InetSocketAddress
import scala.concurrent.Future

object AsyncNetworkApi {
  implicit class NodeAsyncNetworkApi(node: Node) {
    import scala.concurrent.ExecutionContext.Implicits.global

    def nonce: Long = System.currentTimeMillis()

    def sendByNetwork(messages: Any*): Future[Unit] = {
      val sender = new NetworkSender(
        node.settings.networkSettings.trafficLogger,
        node.settings.blockchainSettings.addressSchemeCharacter,
        s"it-client-to-${node.name}",
        nonce
      )
      sender.connect(new InetSocketAddress("localhost", node.nodeExternalPort(node.settings.networkSettings.bindAddress.getPort))).map { ch =>
        if (ch.isActive) sender.send(ch, messages*).map(_ => sender.close()) else sender.close()
      }
    }
  }
}
