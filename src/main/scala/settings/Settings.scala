package settings

import java.net.{InetSocketAddress, InetAddress}
import play.api.libs.json.Json
import scala.util.Try

object Settings {
  val Release = "Lagonaki Release v. 0.9"

  lazy val Port = 9084

  private lazy val settingsJSON = Try {
    val jsonString = scala.io.Source.fromFile("settings.json").mkString
    //CREATE JSON OBJECT
    Json.parse(jsonString)
  }.getOrElse {
    System.out.println("ERROR reading settings.json, closing")
    System.exit(0)
    Json.obj()
  }

  lazy val knownPeers = Try {
    (settingsJSON \ "knownpeers").as[List[String]].flatMap { addr =>
      val inetAddress = InetAddress.getByName(addr)
      if (inetAddress == InetAddress.getLocalHost) None else Some(new InetSocketAddress(inetAddress, Port))
    }
  }.getOrElse(Seq[InetSocketAddress]())
  lazy val maxConnections = (settingsJSON \ "maxconnections").asOpt[Int].getOrElse(DEFAULT_MAX_CONNECTIONS)
  lazy val connectionTimeout = (settingsJSON \ "connectiontimeout").asOpt[Int].getOrElse(DEFAULT_CONNECTION_TIMEOUT)
  lazy val rpcPort = (settingsJSON \ "rpcport").asOpt[Int].getOrElse(DEFAULT_RPC_PORT)
  lazy val rpcAllowed: Seq[String] = (settingsJSON \ "rpcallowed").asOpt[List[String]].getOrElse(DEFAULT_RPC_ALLOWED.split(""))
  lazy val walletDir = (settingsJSON \ "walletdir").asOpt[String].getOrElse(DEFAULT_WALLET_DIR)
  lazy val dataDir = (settingsJSON \ "datadir").asOpt[String].getOrElse(DEFAULT_DATA_DIR)
  lazy val pingInterval = (settingsJSON \ "pinginterval").asOpt[Int].getOrElse(DEFAULT_PING_INTERVAL)
  lazy val maxBytePerFee = (settingsJSON \ "maxbyteperfee").asOpt[Int].getOrElse(DEFAULT_MAX_BYTE_PER_FEE)
  lazy val offlineGeneration = (settingsJSON \ "offline-generation").asOpt[Boolean].getOrElse(false)

  //BLOCKCHAIN
  lazy val maxRollback = 100

  val MaxBlocksChunks = 200

  //NETWORK
  private val DEFAULT_MAX_CONNECTIONS = 20
  private val DEFAULT_CONNECTION_TIMEOUT = 60000
  private val DEFAULT_PING_INTERVAL = 30000

  //RPC
  private val DEFAULT_RPC_PORT = 9085
  private val DEFAULT_RPC_ALLOWED = "127.0.0.1"

  //FOLDERS
  private val DEFAULT_DATA_DIR = "data"
  private val DEFAULT_WALLET_DIR = "wallet"

  private val DEFAULT_MAX_BYTE_PER_FEE = 512
}