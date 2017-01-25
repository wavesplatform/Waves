package com.wavesplatform.settings

import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.{ChainParameters, MainNetParams, TestNetParams}
import play.api.libs.json.JsObject
import scorex.settings.Settings

class WavesSettings(override val settingsJSON: JsObject) extends Settings with MatcherSettings {
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
