package com.wavesplatform.it

import java.net.{InetSocketAddress, URL}
import scala.concurrent.duration.FiniteDuration
import com.typesafe.config.Config
import com.wavesplatform.account.{KeyPair, PublicKey, SeedKeyPair}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.util.GlobalTimer
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.transaction.TransactionType
import com.wavesplatform.utils.LoggerFacade
import com.wavesplatform.wallet.Wallet
import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import org.asynchttpclient.*
import org.asynchttpclient.Dsl.{config as clientConfig, *}
import org.slf4j.LoggerFactory

abstract class Node(val config: Config) extends AutoCloseable {
  lazy val log: LoggerFacade =
    LoggerFacade(LoggerFactory.getLogger(s"${getClass.getCanonicalName}.${this.name}"))

  val settings: WavesSettings = WavesSettings.fromRootConfig(config)
  val client: AsyncHttpClient = asyncHttpClient(
    clientConfig()
      .setKeepAlive(false)
      .setNettyTimer(GlobalTimer.instance)
  )

  lazy val grpcChannel: ManagedChannel = ManagedChannelBuilder
    .forAddress(nodeApiEndpoint.getHost, nodeExternalPort(6870))
    .usePlaintext()
    .build()

  private[this] val wallet = Wallet(settings.walletSettings.copy(file = None))
  wallet.generateNewAccounts(1)

  def generateKeyPair(): SeedKeyPair = wallet.synchronized {
    wallet.generateNewAccount().get
  }

  val keyPair: KeyPair     = KeyPair.fromSeed(config.getString("account-seed")).explicitGet()
  val publicKey: PublicKey = PublicKey.fromBase58String(config.getString("public-key")).explicitGet()
  val address: String      = config.getString("address")

  def nodeExternalPort(internalPort: Int): Int
  def nodeApiEndpoint: URL
  def apiKey: String

  /** An address which can be reached from the host running IT (may not match the declared address) */
  def networkAddress: InetSocketAddress

  override def close(): Unit = client.close()
}

object Node {
  implicit class NodeExt(val n: Node) extends AnyVal {
    def name: String               = n.settings.networkSettings.nodeName
    def publicKeyStr: String       = n.publicKey.toString
    def fee(txTypeId: Byte): Long  = FeeValidation.FeeConstants(TransactionType(txTypeId)) * FeeValidation.FeeUnit
    def blockDelay: FiniteDuration = n.settings.blockchainSettings.genesisSettings.averageBlockDelay
  }
}
