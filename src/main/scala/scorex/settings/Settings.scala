package scorex.settings

import java.net.{InetAddress, InetSocketAddress}

import org.slf4j.LoggerFactory
import play.api.libs.json.Json
import scorex.crypto.Base58

import scala.util.Try

/*
 Changeable settings here
 */

object Settings {
  def logger = LoggerFactory.getLogger(this.getClass)

  var filename = "settings.json"

  lazy val Port = 9084

  private lazy val settingsJSON = Try {
    val jsonString = scala.io.Source.fromFile(filename).mkString
    Json.parse(jsonString)
  }.recover { case _ =>
    val jsonString = scala.io.Source.fromURL(getClass.getResource(s"/$filename")).mkString
    Json.parse(jsonString)
  }.getOrElse {
    logger.info(s"ERROR while reading $filename, closing")
    //catch error?
    System.exit(10)
    Json.obj()
  }

  private def directoryEnsuring(dirPath: String): Boolean = {
    val f = new java.io.File(dirPath)
    f.mkdirs()
    f.exists()
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
  lazy val pingInterval = (settingsJSON \ "pinginterval").asOpt[Int].getOrElse(DEFAULT_PING_INTERVAL)
  lazy val maxBytePerFee = (settingsJSON \ "maxbyteperfee").asOpt[Int].getOrElse(DEFAULT_MAX_BYTE_PER_FEE)
  lazy val offlineGeneration = (settingsJSON \ "offline-generation").asOpt[Boolean].getOrElse(false)
  lazy val bindAddress = (settingsJSON \ "bindAddress").asOpt[String].getOrElse(DEFAULT_BIND_ADDRESS)

  lazy val walletDirOpt = (settingsJSON \ "walletdir").asOpt[String]
    .ensuring(pathOpt => pathOpt.map(directoryEnsuring).getOrElse(true))

  lazy val walletPassword = (settingsJSON \ "walletpassword").as[String]

  lazy val walletSeed = Base58.decode((settingsJSON \ "walletseed").as[String])

  lazy val dataDirOpt = {
    val res = (settingsJSON \ "datadir").asOpt[String]
    res.foreach(folder => new java.io.File(folder).mkdirs())
    res
  }

  //BLOCKCHAIN
  lazy val maxRollback = 100

  val MaxBlocksChunks = 5

  //NETWORK
  private val DEFAULT_MAX_CONNECTIONS = 20
  private val DEFAULT_CONNECTION_TIMEOUT = 60000
  private val DEFAULT_PING_INTERVAL = 30000
  private val DEFAULT_BIND_ADDRESS = "127.0.0.1"

  //RPC
  private val DEFAULT_RPC_PORT = 9085
  private val DEFAULT_RPC_ALLOWED = "127.0.0.1"

  //FOLDERS
  private val DEFAULT_DATA_DIR = "data"
  private val DEFAULT_WALLET_DIR = "wallet"

  private val DEFAULT_MAX_BYTE_PER_FEE = 512
}