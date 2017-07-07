package com.wavesplatform.it

import com.typesafe.config.Config
import com.wavesplatform.it.api._
import com.wavesplatform.settings.WavesSettings
import org.asynchttpclient._
import org.slf4j.LoggerFactory
import scorex.transaction.TransactionParser.TransactionType
import scorex.utils.LoggerFacade

import scala.concurrent.duration.FiniteDuration


class Node(config: Config, val nodeInfo: NodeInfo, override val client: AsyncHttpClient)
  extends NodeApi with NetworkNodeApi {
  val privateKey: String = config.getString("private-key")
  val publicKey: String = config.getString("public-key")
  val address: String = config.getString("address")
  val accountSeed: String = config.getString("account-seed")
  val settings: WavesSettings = WavesSettings.fromConfig(config)

  override protected val log = LoggerFacade(LoggerFactory.getLogger(s"${getClass.getName}.${settings.networkSettings.nodeName}"))

  override val chainId: Char = 'I'
  override val nodeName: String = s"client-to-${nodeInfo.networkIpAddress}"
  override val restAddress: String = "localhost"
  override val networkAddress: String = nodeInfo.networkIpAddress
  override val nodeRestPort: Int = nodeInfo.hostRestApiPort
  override val matcherRestPort: Int = nodeInfo.hostMatcherApiPort
  override val networkPort: Int = nodeInfo.hostNetworkPort
  override val blockDelay: FiniteDuration = settings.blockchainSettings.genesisSettings.averageBlockDelay

  def fee(txValue: TransactionType.Value, asset: String = "WAVES"): Long =
    settings.feesSettings.fees(txValue.id).find(_.asset == asset).get.fee
}
