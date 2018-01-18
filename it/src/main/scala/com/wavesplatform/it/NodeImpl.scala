package com.wavesplatform.it

import com.typesafe.config.Config
import com.wavesplatform.it.api._
import com.wavesplatform.it.util.GlobalTimer
import com.wavesplatform.settings.WavesSettings
import org.asynchttpclient._
import scorex.transaction.TransactionParser.TransactionType

import scala.concurrent.duration.FiniteDuration

class NodeImpl(val config: Config, var nodeInfo: NodeInfo, override val client: AsyncHttpClient) extends Node {
  val privateKey: String = config.getString("private-key")
  val publicKey: String = config.getString("public-key")
  val address: String = config.getString("address")
  val accountSeed: String = config.getString("account-seed")
  val settings: WavesSettings = WavesSettings.fromConfig(config)

  def name: String = settings.networkSettings.nodeName
  val restAddress: String = "localhost"

  override def restEndpoint = nodeInfo.restApi

  override def matcherRestEndpoint = nodeInfo.matcherApi

  def networkPort: Int = nodeInfo.hostNetworkPort

  val blockDelay: FiniteDuration = settings.blockchainSettings.genesisSettings.averageBlockDelay

  def fee(txValue: TransactionType.Value, asset: String): Long =
    settings.feesSettings.fees(txValue.id).find(_.asset == asset).get.fee
}

object NodeImpl {
  def apply(config: Config): Node = new NodeImpl(
    config,
    NodeInfo(
      config.getString("rest-api"),
      config.getString("matcher-api"),
      config.getInt("network-port"),
      config.getInt("network-port"),
      config.getString("hostname"),
      "",
      config.getString("api-key")
    ),
    Dsl.asyncHttpClient(Dsl.config().setNettyTimer(GlobalTimer.instance))
  ) {
    override val restAddress: String = config.getString("hostname")
  }
}
