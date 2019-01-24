package com.wavesplatform.it

import java.net.{InetSocketAddress, URL}

import com.typesafe.config.Config
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.util.GlobalTimer
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.utils.LoggerFacade
import org.asynchttpclient.Dsl.{config => clientConfig, _}
import org.asynchttpclient._
import org.slf4j.LoggerFactory

import scala.concurrent.duration.FiniteDuration

abstract class Node(config: Config) extends AutoCloseable {
  lazy val log: LoggerFacade =
    LoggerFacade(LoggerFactory.getLogger(s"${getClass.getCanonicalName}.${this.name}"))

  val settings: WavesSettings = WavesSettings.fromConfig(config)
  val client: AsyncHttpClient = asyncHttpClient(
    clientConfig()
      .setKeepAlive(false)
      .setNettyTimer(GlobalTimer.instance))

  val privateKey: PrivateKeyAccount = PrivateKeyAccount.fromSeed(config.getString("account-seed")).explicitGet()
  val publicKey: PublicKeyAccount   = PublicKeyAccount.fromBase58String(config.getString("public-key")).explicitGet()
  val address: String               = config.getString("address")

  def nodeApiEndpoint: URL
  def matcherApiEndpoint: URL
  def apiKey: String

  /** An address which can be reached from the host running IT (may not match the declared address) */
  def networkAddress: InetSocketAddress

  override def close(): Unit = client.close()
}

object Node {
  implicit class NodeExt(val n: Node) extends AnyVal {
    def name: String = n.settings.networkSettings.nodeName

    def publicKeyStr = Base58.encode(n.publicKey.publicKey)

    def fee(txTypeId: Byte): Long = CommonValidation.FeeConstants(txTypeId) * CommonValidation.FeeUnit

    def blockDelay: FiniteDuration = n.settings.blockchainSettings.genesisSettings.averageBlockDelay
  }
}
