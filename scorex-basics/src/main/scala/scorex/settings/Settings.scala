package scorex.settings

import java.io.File
import java.net.InetSocketAddress

import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58
import scorex.utils.ScorexLogging

import scala.concurrent.duration._
import scala.util.{Random, Try}

/**
  * Settings
  */

trait Settings extends ScorexLogging {

  val filename: String

  //TODO: gagarin55 - remove this initialization away from trait!
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

  lazy val dataDirOpt = {
    val res = (settingsJSON \ "dataDir").asOpt[String]
    res.foreach(folder => new File(folder).mkdirs())
    require(res.isEmpty || new File(res.get).exists())
    res
  }

  //p2p
  lazy val DefaultPort = 9084

  lazy val p2pSettings = settingsJSON \ "p2p"

  lazy val nodeNonce: Long = (Random.nextInt(1000) + 1000) * Random.nextInt(1000) + Random.nextInt(1000)
  lazy val nodeName = (p2pSettings \ "nodeName").asOpt[String]
    .getOrElse(Random.nextPrintableChar().toString + nodeNonce)

  private val DefaultBlacklistResidenceTimeMilliseconds: Long = 10 * 60 * 1000

  lazy val blacklistResidenceTimeMilliseconds: Long =
    (p2pSettings \ "blacklistResidenceTimeMilliseconds").asOpt[Long]
      .getOrElse(DefaultBlacklistResidenceTimeMilliseconds)

  lazy val localOnly = (p2pSettings \ "localOnly").asOpt[Boolean].getOrElse(false)

  lazy val knownPeers = Try {
    (p2pSettings \ "knownPeers").as[List[String]].map { addr =>
      val addrParts = addr.split(":").map(_.trim)
      val port = if (addrParts.length == 2) addrParts(1).toInt else DefaultPort
      new InetSocketAddress(addrParts(0), port)
    }
  }.getOrElse(Seq[InetSocketAddress]())
  lazy val bindAddress = (p2pSettings \ "bindAddress").asOpt[String].getOrElse(DefaultBindAddress)
  lazy val maxConnections = (p2pSettings \ "maxConnections").asOpt[Int].getOrElse(DefaultMaxConnections)
  lazy val connectionTimeout = (p2pSettings \ "connectionTimeout").asOpt[Int].getOrElse(DefaultConnectionTimeout)
  lazy val upnpEnabled = (p2pSettings \ "upnp").asOpt[Boolean].getOrElse(true)
  lazy val upnpGatewayTimeout = (p2pSettings \ "upnpGatewayTimeout").asOpt[Int]
  lazy val upnpDiscoverTimeout = (p2pSettings \ "upnpDiscoverTimeout").asOpt[Int]
  lazy val port = (p2pSettings \ "port").asOpt[Int].getOrElse(DefaultPort)
  lazy val declaredAddress = (p2pSettings \ "myAddress").asOpt[String]
  lazy val fuzzingDelay = (p2pSettings \ "fuzzingDelay").asOpt[Int].getOrElse(0)
  lazy val minEphemeralPortNumber = (p2pSettings \ "minEphemeralPortNumber").asOpt[Int].getOrElse(32768)
  lazy val peersDataBroadcastDelay = (p2pSettings \ "peersDataBroadcastDelay").asOpt[Long]
    .map(x => FiniteDuration(x, MILLISECONDS)).getOrElse(30.seconds)


  //p2p settings assertions
  assert(!(localOnly && upnpEnabled), "Both localOnly and upnp enabled")
  //todo: localOnly & declaredAddress

  lazy val rpcPort = (settingsJSON \ "rpcPort").asOpt[Int].getOrElse(DefaultRpcPort)
  lazy val rpcAddress = (settingsJSON \ "rpcAddress").asOpt[String].getOrElse(DefaultRpcAddress)

  lazy val offlineGeneration = (settingsJSON \ "offlineGeneration").asOpt[Boolean].getOrElse(false)

  // Blockchain download & sync settings
  private val DefaultMaxRollback = 100
  lazy val MaxRollback = (settingsJSON \ "maxRollback").asOpt[Int].getOrElse(DefaultMaxRollback)
  val MaxBlocksChunks = 10
  lazy val forkMaxLength = (settingsJSON \ "forkMaxLength").asOpt[Int].getOrElse(DefaultMaxRollback + 1)
  lazy val forkResolveQuorumSize = (settingsJSON \ "forkResolveQuorumSize").asOpt[Int].getOrElse(1)
  lazy val forkFileName = (settingsJSON \ "forkFileName").asOpt[String]
  lazy val minForkChunkSize = (settingsJSON \ "minForkChunkSize").asOpt[Int].getOrElse(1)
  lazy val loadEntireForkChunk = (settingsJSON \ "loadEntireForkChunk").asOpt[Boolean].getOrElse(true)

  // Blockchain download & sync retry settings
  lazy val historySynchronizerTimeout: FiniteDuration = (settingsJSON \ "historySynchronizerTimeout").asOpt[Int]
    .map(x => x.seconds).getOrElse(DefaultHistorySynchronizerTimeout)
  lazy val pinToInitialPeer = (settingsJSON \ "pinToInitialPeer").asOpt[Boolean].getOrElse(true)
  lazy val retriesBeforeBlacklisted = (settingsJSON \ "retriesBeforeBlacklisted").asOpt[Int].getOrElse(2)
  lazy val operationRetries = (settingsJSON \ "operationRetries").asOpt[Int].getOrElse(
    if (pinToInitialPeer) retriesBeforeBlacklisted + 1 else forkResolveQuorumSize)

  // Miner settings
  lazy val blockGenerationDelay: FiniteDuration = (settingsJSON \ "blockGenerationDelay").asOpt[Long]
    .map(x => FiniteDuration(x, MILLISECONDS)).getOrElse(DefaultBlockGenerationDelay)
  lazy val mininigThreads: Int = (settingsJSON \ "mininigThreads").asOpt[Int].getOrElse(DefaultMiningThreads)
  lazy val tflikeScheduling = (settingsJSON \ "tflikeScheduling").asOpt[Boolean].getOrElse(true)

  lazy val maxPeersToBroadcastBlock = (settingsJSON \ "maxPeersToBroadcastBlock").asOpt[Int].getOrElse(1)

  val scoreTTL: FiniteDuration = 1.minute
  lazy val scoreBroadcastDelay: FiniteDuration = (settingsJSON \ "scoreBroadcastDelay").asOpt[Long]
    .map(x => FiniteDuration(x, MILLISECONDS)).getOrElse(30.seconds)

  lazy val walletDirOpt = (settingsJSON \ "walletDir").asOpt[String]
    .ensuring(pathOpt => pathOpt.map(directoryEnsuring).getOrElse(true))
  lazy val walletPassword = (settingsJSON \ "walletPassword").asOpt[String].getOrElse {
    println("Please type your wallet password")
    scala.io.StdIn.readLine()
  }
  lazy val walletSeed = (settingsJSON \ "walletSeed").asOpt[String].flatMap(s => Base58.decode(s).toOption)

  lazy val apiKeyHash = (settingsJSON \ "apiKeyHash").asOpt[String].flatMap(s => Base58.decode(s).toOption)

  lazy val genesisTimestamp: Long = (settingsJSON \ "genesisTimestamp").asOpt[Long].getOrElse(DefaultGenesisTimestamp)

  //NETWORK
  private val DefaultMaxConnections = 20
  private val DefaultConnectionTimeout = 60
  private val DefaultBindAddress = "127.0.0.1"

  //API
  lazy val corsAllowed = (settingsJSON \ "cors").asOpt[Boolean].getOrElse(false)

  private val DefaultRpcPort = 9085
  private val DefaultRpcAddress = "0.0.0.0"
  private val DefaultRpcAllowed = "127.0.0.1"

  private val DefaultBlockGenerationDelay: FiniteDuration = 1.second
  private val DefaultHistorySynchronizerTimeout: FiniteDuration = 30.seconds
  private val DefaultMiningThreads: Int = 1

  private val DefaultGenesisTimestamp: Long = 1460952000000L
}
