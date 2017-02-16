package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.matcher.MatcherSettings
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.EnumerationReader._

object LogLevel extends Enumeration {
  val DEBUG = Value("DEBUG")
  val INFO = Value("INFO")
  val WARN = Value("WARN")
  val ERROR = Value("ERROR")
}

case class WavesSettings(directory: String,
                         loggingLevel: LogLevel.Value,
                         networkSettings: NetworkSettings,
                         walletSettings: WalletSettings,
                         blockchainSettings: BlockchainSettings,
                         checkpointsSettings: CheckpointsSettings,
                         feesSettings: FeesSettings,
                         matcherSettings: MatcherSettings,
                         minerSettings: MinerSettings,
                         restAPISettings: RestAPISettings,
                         synchronizationSettings: SynchronizationSettings,
                         utxSettings: UTXSettings)

object WavesSettings {
  val configPath: String = "waves"
  def fromConfig(config: Config): WavesSettings = {
    val directory = config.as[String](s"$configPath.directory")
    val loggingLevel = config.as[LogLevel.Value](s"$configPath.logging-level")

    val networkSettings = NetworkSettings.fromConfig(config)
    val walletSettings = WalletSettings.fromConfig(config)
    val blockchainSettings = BlockchainSettings.fromConfig(config)
    val checkpointsSettings = CheckpointsSettings.fromConfig(config)
    val feesSettings = FeesSettings.fromConfig(config)
    val matcherSettings = MatcherSettings.fromConfig(config)
    val minerSettings = MinerSettings.fromConfig(config)
    val restAPISettings = RestAPISettings.fromConfig(config)
    val synchronizationSettings = SynchronizationSettings.fromConfig(config)
    val utxSettings = UTXSettings.fromConfig(config)

    WavesSettings(directory, loggingLevel, networkSettings, walletSettings, blockchainSettings, checkpointsSettings,
      feesSettings, matcherSettings, minerSettings, restAPISettings, synchronizationSettings, utxSettings)
  }
}

/*
                          override val settingsJSON: JsObject) extends Settings {
  lazy val loggingLevel: String = (settingsJSON \ "loggingLevel").asOpt[String].getOrElse("info").toLowerCase

  override lazy val genesisTimestamp = 1460678400000L
  override val MaxBlocksChunks = 20

  lazy val minimumTxFee: Long = (settingsJSON \ "minimumTxFee").asOpt[Long].getOrElse(DefaultMinimumTxFee)
  private val DefaultMinimumTxFee = 100000L

  lazy val isTestNet: Boolean = (settingsJSON \ "testnet").asOpt[Boolean].getOrElse(true)
  lazy val chainParams: ChainParameters = if (isTestNet) TestNetParams else MainNetParams

  // blockchain and wallet directories
  lazy val DefaultDataDir: String = System.getProperty("user.home") + s"/${Constants.ApplicationName}/"
  override lazy val dataDirOpt: Option[String] = getDir("dataDir", "data")
  override lazy val walletDirOpt: Option[String] = getDir("walletDir", "wallet")

  /**
    * Returns directory path from config or DefaultDataDir/@dirName by default
    * Creates directory if it doesn't exist
    */
  def getDir(param: String, dirName: String): Some[String] = {
    val path = (settingsJSON \ param).asOpt[String] match {
      case Some(x) if x.length > 0 => Some(x)
      case _ => Some(DefaultDataDir + dirName)
    }
    path.ensuring(pathOpt => pathOpt.forall(directoryEnsuring), s"Can't create or read folder '$path'")
  }
}
*/
