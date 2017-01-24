package scorex.settings

import java.net.InetSocketAddress

import play.api.libs.json.{JsLookupResult, JsObject, Json}
import scorex.crypto.encode.Base58
import scorex.transaction.{AssetId, TransactionAssetFee}
import scorex.utils.ScorexLogging

import scala.concurrent.duration._
import scala.util.{Random, Try}

/**
  * Settings
  */

trait Settings extends ScorexLogging {

  def settingsJSON: JsObject

  protected def directoryEnsuring(dirPath: String): Boolean = {
    val f = new java.io.File(dirPath)
    f.mkdirs()
    f.exists()
  }

  lazy val dataDirOpt: Option[String] = (settingsJSON \ "dataDir").asOpt[String]
    .ensuring(pathOpt => pathOpt.forall(directoryEnsuring))

  lazy val walletDirOpt: Option[String] = (settingsJSON \ "walletDir").asOpt[String]
    .ensuring(pathOpt => pathOpt.forall(directoryEnsuring))

  //p2p
  lazy val DefaultPort: Int = 9084

  lazy val p2pSettings: JsLookupResult = settingsJSON \ "p2p"

  lazy val nodeNonce: Long = (Random.nextInt(1000) + 1000) * Random.nextInt(1000) + Random.nextInt(1000)

  lazy val nodeName: String = (p2pSettings \ "nodeName").asOpt[String]
    .getOrElse(Random.nextPrintableChar().toString + nodeNonce)

  private val DefaultBlacklistResidenceTimeMilliseconds: Long = 10 * 60 * 1000

  lazy val blacklistResidenceTimeMilliseconds: Long =
    (p2pSettings \ "blacklistResidenceTimeMilliseconds").asOpt[Long]
      .getOrElse(DefaultBlacklistResidenceTimeMilliseconds)

  lazy val peersDataResidenceTime: FiniteDuration =
    (p2pSettings \ "peersDataResidenceTimeDays").asOpt[Int].getOrElse(1).days

  lazy val localOnly: Boolean = (p2pSettings \ "localOnly").asOpt[Boolean].getOrElse(false)

  lazy val knownPeers: Seq[InetSocketAddress] = Try {
    (p2pSettings \ "knownPeers").as[List[String]].map { addr =>
      val addrParts = addr.split(":").map(_.trim)
      val port = if (addrParts.length == 2) addrParts(1).toInt else DefaultPort
      new InetSocketAddress(addrParts(0), port)
    }
  }.getOrElse(Seq[InetSocketAddress]())
  lazy val bindAddress: String = (p2pSettings \ "bindAddress").asOpt[String].getOrElse(DefaultBindAddress)
  lazy val maxConnections: Int = (p2pSettings \ "maxConnections").asOpt[Int].getOrElse(DefaultMaxConnections)
  lazy val connectionTimeout: Int = (p2pSettings \ "connectionTimeout").asOpt[Int].getOrElse(DefaultConnectionTimeout)
  lazy val upnpEnabled: Boolean = (p2pSettings \ "upnp").asOpt[Boolean].getOrElse(true)
  lazy val upnpGatewayTimeout: Option[Int] = (p2pSettings \ "upnpGatewayTimeout").asOpt[Int]
  lazy val upnpDiscoverTimeout: Option[Int] = (p2pSettings \ "upnpDiscoverTimeout").asOpt[Int]
  lazy val port: Int = (p2pSettings \ "port").asOpt[Int].getOrElse(DefaultPort)
  lazy val declaredAddress: Option[String] = (p2pSettings \ "myAddress").asOpt[String]
  lazy val fuzzingDelay: Int = (p2pSettings \ "fuzzingDelay").asOpt[Int].getOrElse(0)
  lazy val outboundBufferSize: Int = (p2pSettings \ "outboundBufferSizeMb").asOpt[Int].getOrElse(15) * 1024 * 1024
  lazy val minEphemeralPortNumber: Int = (p2pSettings \ "minEphemeralPortNumber").asOpt[Int].getOrElse(32768)
  lazy val acceptExternalPeerData: Boolean = (p2pSettings \ "acceptExternalPeerData").asOpt[Boolean].getOrElse(true)
  lazy val MaxUnverifiedPeers: Int = (p2pSettings \ "maxUnverifiedPeers").asOpt[Int].getOrElse(1000)
  lazy val peersDataBroadcastDelay: FiniteDuration = (p2pSettings \ "peersDataBroadcastDelay").asOpt[Long]
    .map(x => FiniteDuration(x, MILLISECONDS)).getOrElse(30.seconds)


  //p2p settings assertions
  assert(!(localOnly && upnpEnabled), "Both localOnly and upnp enabled")
  //todo: localOnly & declaredAddress

  lazy val rpcPort: Int = (settingsJSON \ "rpcPort").asOpt[Int].getOrElse(DefaultRpcPort)
  lazy val rpcAddress: String = (settingsJSON \ "rpcAddress").asOpt[String].getOrElse(DefaultRpcAddress)

  lazy val offlineGeneration: Boolean = (settingsJSON \ "offlineGeneration").asOpt[Boolean].getOrElse(false)

  lazy val MaxRollback: Int = (settingsJSON \ "maxRollback").asOpt[Int].getOrElse(100)
  val MaxBlocksChunks = 10
  lazy val maxChain: Int = (settingsJSON \ "maxChain").asOpt[Int].getOrElse(MaxRollback + 1)
  lazy val quorum: Int = (settingsJSON \ "quorum").asOpt[Int].getOrElse(1)
  lazy val chainFileName: Option[String] = (settingsJSON \ "chainFileName").asOpt[String]
  lazy val loadEntireChain: Boolean = (settingsJSON \ "loadEntireChain").asOpt[Boolean].getOrElse(true)
  lazy val blacklistThreshold: Int = (settingsJSON \ "blacklistThreshold").asOpt[Int].getOrElse(50)

  assert(maxChain > 1, "maxChain value should be 2 or more")

  // Blockchain download & sync retry settings
  lazy val historySynchronizerTimeout: FiniteDuration = (settingsJSON \ "historySynchronizerTimeout").asOpt[Int]
    .map(x => x.seconds).getOrElse(DefaultHistorySynchronizerTimeout)
  lazy val pinToInitialPeer: Boolean = (settingsJSON \ "pinToInitialPeer").asOpt[Boolean].getOrElse(false)
  lazy val retriesBeforeBlacklisted: Int = (settingsJSON \ "retriesBeforeBlacklisted").asOpt[Int].getOrElse(2)
  lazy val operationRetries: Int = (settingsJSON \ "operationRetries").asOpt[Int].getOrElse(
    if (pinToInitialPeer) retriesBeforeBlacklisted + 1 else maxConnections)

  // Miner settings
  lazy val blockGenerationDelay: FiniteDuration = (settingsJSON \ "blockGenerationDelay").asOpt[Long]
    .map(x => FiniteDuration(x, MILLISECONDS)).getOrElse(DefaultBlockGenerationDelay)

  lazy val tflikeScheduling: Boolean = (settingsJSON \ "tflikeScheduling").asOpt[Boolean].getOrElse(true)

  lazy val scoreBroadcastDelay: FiniteDuration = (settingsJSON \ "scoreBroadcastDelay").asOpt[Long]
    .map(x => FiniteDuration(x, MILLISECONDS)).getOrElse(30.seconds)
  lazy val scoreTTL: FiniteDuration = scoreBroadcastDelay * 3

  lazy val walletPassword: String = (settingsJSON \ "walletPassword").asOpt[String].getOrElse {
    println("Please type your wallet password")
    scala.io.StdIn.readLine()
  }
  lazy val walletSeed: Option[Array[Byte]] = (settingsJSON \ "walletSeed").asOpt[String].flatMap(s => Base58.decode(s).toOption)

  lazy val apiKeyHash: Option[Array[Byte]] = (settingsJSON \ "apiKeyHash").asOpt[String].flatMap(s => Base58.decode(s).toOption)

  lazy val genesisTimestamp: Long = (settingsJSON \ "genesisTimestamp").asOpt[Long].getOrElse(DefaultGenesisTimestamp)

  lazy val genesisSignature: Option[String] = (settingsJSON \ "genesisSignature").asOpt[String]

  lazy val allowedGenerationTimeFromLastBlockInterval: FiniteDuration = (settingsJSON \ "allowedGenerationTimeFromLastBlockInterval").asOpt[Long]
    .map(x => FiniteDuration(x, MILLISECONDS)).getOrElse(DefaultAllowedGenerationTimeFromLastBlockInterval)

  lazy val minerEnabled: Boolean = (settingsJSON \ "minerEnabled").asOpt[Boolean].getOrElse(true)

  lazy val checkpointSettings: JsLookupResult = settingsJSON \ "checkpoints"
  lazy val checkpointPublicKey: Option[Array[Byte]] = (checkpointSettings \ "publicKey").asOpt[String].flatMap(Base58.decode(_).toOption)

  lazy val history: String = (settingsJSON \ "history").asOpt[String].getOrElse(DefaultHistory)

  lazy val utxRebroadcastInterval: FiniteDuration = (settingsJSON \ "utxRebroadcastInterval").asOpt[Int]
    .map(x => x.seconds).getOrElse(DefaultUtxRebroadcastInterval)

  lazy val utxSize: Int = (settingsJSON \ "utxSize").asOpt[Int].getOrElse(DefaultUTXSize)

  lazy val feeMap: Map[String, Long] = (settingsJSON \ "feeMap").validate[Map[String, Map[String, Long]]].map(_.flatMap { e =>
    e._2.map { kv =>
      val assetId: Option[AssetId] = if (kv._1 == "Waves") None else Some(Base58.decode(kv._1).get)
      TransactionAssetFee(e._1.toInt, assetId).key -> kv._2
    }
  }).getOrElse(DefaultFeeMap)

  private val DefaultFeeMap: Map[String, Long] = Map(
    TransactionAssetFee(2, None).key -> 100000,
    TransactionAssetFee(3, None).key -> 100000000,
    TransactionAssetFee(4, None).key -> 100000,
    TransactionAssetFee(5, None).key -> 100000,
    TransactionAssetFee(6, None).key -> 100000,
    TransactionAssetFee(7, None).key -> 100000,
    TransactionAssetFee(8, None).key -> 100000
  )

  private val DefaultHistory = "blockchain"
  private val DefaultUtxRebroadcastInterval: FiniteDuration = 30.seconds
  private val DefaultUTXSize = 10000

  //NETWORK
  private val DefaultMaxConnections = 30
  private val DefaultConnectionTimeout = 60
  private val DefaultBindAddress = "127.0.0.1"
  lazy val AllowedConnectionsFromOneHost = 5
  lazy val UnrequestedPacketsThreshold = 100
  private val DefaultAllowedGenerationTimeFromLastBlockInterval: FiniteDuration = 1.day


  //API
  lazy val corsAllowed: Boolean = (settingsJSON \ "cors").asOpt[Boolean].getOrElse(false)
  lazy val rpcEnabled: Boolean = (settingsJSON \ "rpcEnabled").asOpt[Boolean].getOrElse(true)

  private val DefaultRpcPort = 9085
  private val DefaultRpcAddress = "127.0.0.1"

  private val DefaultBlockGenerationDelay: FiniteDuration = 1.second
  private val DefaultHistorySynchronizerTimeout: FiniteDuration = 30.seconds

  private val DefaultGenesisTimestamp: Long = 1460952000000L
}

object Settings extends ScorexLogging {
  lazy val empty = new Settings {
    override def settingsJSON: JsObject = Json.obj()
  }

  def readSettingsJson(filename: String): JsObject = {
    Try {
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
  }

  def apply(filename: String): Settings = {
    val json = readSettingsJson(filename)
    new Settings {
      override def settingsJSON: JsObject = json
    }
  }
}
