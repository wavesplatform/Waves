package com.wavesplatform.it.api

import java.net.InetSocketAddress

import com.wavesplatform.it.util.NetworkSender
import com.wavesplatform.network.RawBytes

import scala.concurrent.Future

trait NetworkNodeApi {
  def networkAddress: String
  def networkPort: Int
  def chainId: Char
  def nodeName: String
  def nonce: Long = System.currentTimeMillis()

  def sendByNetwork(message: RawBytes*): Future[Unit] = {
    val c = new NetworkSender(new InetSocketAddress(networkAddress, networkPort), chainId, nodeName, nonce)
    try {
      c.sendByNetwork(message: _*)
    } finally {
      c.close()
    }
  }
}
