package com.wavesplatform.it.api

import java.net.InetSocketAddress

import com.wavesplatform.it.util.NetworkSender
import com.wavesplatform.network.RawBytes

import scala.concurrent.Future

trait NetworkNodeApi {

  import scala.concurrent.ExecutionContext.Implicits.global

  def networkAddress: String
  def networkPort: Int
  def chainId: Char
  def nodeName: String
  def nonce: Long = System.currentTimeMillis()

  def sendByNetwork(message: RawBytes*): Future[Unit] = {
    val sender = new NetworkSender(chainId, nodeName, nonce)
    sender.connect(new InetSocketAddress(networkAddress, networkPort)).map { ch =>
      if (ch.isActive) sender.send(ch, message: _*).map(_ => sender.close()) else sender.close()
    }
  }
}
