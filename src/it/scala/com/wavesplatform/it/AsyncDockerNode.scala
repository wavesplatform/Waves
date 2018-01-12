package com.wavesplatform.it

import com.typesafe.config.Config
import com.wavesplatform.it.api._
import com.wavesplatform.settings.WavesSettings
import org.asynchttpclient._
import org.slf4j.LoggerFactory
import scorex.transaction.TransactionParser.TransactionType
import scorex.utils.LoggerFacade

import scala.concurrent.duration.FiniteDuration

class AsyncDockerNode(val config: Config, var nodeInfo: NodeInfo, override val client: AsyncHttpClient)
  extends Node with AsyncNodeHttpApi with AsyncNetworkNodeApi {
  val privateKey: String = config.getString("private-key")
  val publicKey: String = config.getString("public-key")
  val address: String = config.getString("address")
  val accountSeed: String = config.getString("account-seed")
  val settings: WavesSettings = WavesSettings.fromConfig(config)

  override protected val log = LoggerFacade(LoggerFactory.getLogger(s"${getClass.getName}.${settings.networkSettings.nodeName}"))

  override val chainId: Char = 'I'
  override val nodeName: String = s"it-test-client-to-${nodeInfo.networkIpAddress}"
  override val restAddress: String = "localhost"
  override def nodeRestPort: Int = nodeInfo.hostRestApiPort
  override def matcherRestPort: Int = nodeInfo.hostMatcherApiPort
  override def networkPort: Int = nodeInfo.hostNetworkPort

  override val blockDelay: FiniteDuration = settings.blockchainSettings.genesisSettings.averageBlockDelay

  def fee(txValue: TransactionType.Value, asset: String = "WAVES"): Long =
    settings.feesSettings.fees(txValue.id).find(_.asset == asset).get.fee
}
