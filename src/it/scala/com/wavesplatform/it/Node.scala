package com.wavesplatform.it

import java.net.InetSocketAddress

import com.typesafe.config.Config
import com.wavesplatform.it.api._
import com.wavesplatform.it.util._
import com.wavesplatform.network.RawBytes
import com.wavesplatform.settings.WavesSettings
import io.netty.util.Timer
import org.asynchttpclient._
import org.slf4j.LoggerFactory
import scorex.transaction.TransactionParser.TransactionType
import scorex.utils.LoggerFacade

import scala.concurrent.Future


class Node(config: Config, val nodeInfo: NodeInfo, override val client: AsyncHttpClient, override val timer: Timer) extends NodeApi(
  "localhost",
  nodeInfo.hostRestApiPort,
  nodeInfo.hostMatcherApiPort,
  WavesSettings.fromConfig(config).blockchainSettings.genesisSettings.averageBlockDelay
) {
  val privateKey: String = config.getString("private-key")
  val publicKey: String = config.getString("public-key")
  val address: String = config.getString("address")
  val accountSeed: String = config.getString("account-seed")
  val settings: WavesSettings = WavesSettings.fromConfig(config)

  def fee(txValue: TransactionType.Value, asset: String = "WAVES"): Long =
    settings.feesSettings.fees(txValue.id).find(_.asset == asset).get.fee

  override protected val log = LoggerFacade(LoggerFactory.getLogger(s"${getClass.getName}.${settings.networkSettings.nodeName}"))

  def sendByNetwork(message: RawBytes*): Future[Unit] = {
    val c = new NetworkSender(new InetSocketAddress("localhost", nodeInfo.hostNetworkPort), 'I', "it-test-client", System.currentTimeMillis())
    try {
      c.sendByNetwork(message: _*)
    } finally {
      c.close
    }
  }

  override def close: Unit = {}
}
