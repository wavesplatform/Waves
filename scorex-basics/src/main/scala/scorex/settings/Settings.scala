package scorex.settings

import java.net.{InetAddress, InetSocketAddress}

import play.api.libs.json.{JsObject, Json}
import scorex.crypto.Base58
import scorex.utils.ScorexLogging

import scala.util.Try

/**
 * Changeable settings here
 */

trait Settings extends ScorexLogging {

  lazy val Port = 9084

  val filename:String

  lazy val settingsJSON: JsObject = Try {
    val jsonString = scala.io.Source.fromFile(filename).mkString
    Json.parse(jsonString).as[JsObject]
  }.recoverWith { case t =>
    Try {
      val jsonString = scala.io.Source.fromURL(getClass.getResource(s"/$filename")).mkString
      Json.parse(jsonString).as[JsObject]
    }
  }.getOrElse {
    log.error(s"Unable to read $filename, closing")
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

  lazy val maxConnections = (settingsJSON \ "maxconnections").asOpt[Int].getOrElse(DefaultMaxConnections)
  lazy val connectionTimeout = (settingsJSON \ "connectiontimeout").asOpt[Int].getOrElse(DefaultConnectionTimeout)
  lazy val rpcPort = (settingsJSON \ "rpcport").asOpt[Int].getOrElse(DefaultRpcPort)
  lazy val rpcAllowed: Seq[String] = (settingsJSON \ "rpcallowed").asOpt[List[String]].getOrElse(DefaultRpcAllowed.split(""))
  lazy val pingInterval = (settingsJSON \ "pinginterval").asOpt[Int].getOrElse(DefaultPingInterval)
  lazy val offlineGeneration = (settingsJSON \ "offline-generation").asOpt[Boolean].getOrElse(false)
  lazy val bindAddress = (settingsJSON \ "bindAddress").asOpt[String].getOrElse(DefaultBindAddress)

  lazy val walletDirOpt = (settingsJSON \ "walletdir").asOpt[String]
    .ensuring(pathOpt => pathOpt.map(directoryEnsuring).getOrElse(true))

  lazy val walletPassword = (settingsJSON \ "walletpassword").as[String]

  lazy val walletSeed = Base58.decode((settingsJSON \ "walletseed").as[String])

  //NETWORK
  private val DefaultMaxConnections = 20
  private val DefaultConnectionTimeout = 60
  private val DefaultPingInterval = 30000
  private val DefaultBindAddress = "127.0.0.1"

  val MaxBlocksChunks = 5

  //API
  private val DefaultRpcPort = 9085
  private val DefaultRpcAllowed = "127.0.0.1"
}