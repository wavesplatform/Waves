package com.wavesplatform.settings

import com.wavesplatform.{ChainParameters, MainNetParams, TestNetParams}
import scorex.settings.Settings
import scorex.transaction.TransactionSettings
import scala.util.Try
import play.api.libs.json.JsObject

class WavesSettings(override val settingsJSON: JsObject) extends Settings with TransactionSettings {
  lazy val loggingLevel = (settingsJSON \ "loggingLevel").asOpt[String].getOrElse("info").toLowerCase

  override lazy val genesisTimestamp = 1460678400000L
  override val MaxBlocksChunks = 20

  // TODO: Should be moved to Scorex TransactionSettings
  lazy val minimumTxFee = (settingsJSON \ "minimumTxFee").asOpt[Long].getOrElse(DefaultMinimumTxFee)
  private val DefaultMinimumTxFee = 100000L

  val suspendedSenders = Try {
    (settingsJSON \ "suspendedSenders").as[List[String]]
  }.getOrElse(List[String]())

  lazy val isTestNet: Boolean = (settingsJSON \ "testnet").asOpt[Boolean].getOrElse(true)
  lazy val chainParams: ChainParameters = if (isTestNet) TestNetParams else MainNetParams

  // blockchain and wallet directories
  lazy val DefaultDataDir = System.getProperty("user.home") + s"/${Constants.ApplicationName}/"
  override lazy val dataDirOpt: Option[String] = getDir("dataDir", "data")
  override lazy val walletDirOpt: Option[String]  = getDir("walletDir", "wallet")

  /**
    * Returns directory path from config or DefaultDataDir/@dirName by default
    * Creates directory if it doesn't exist
    */
  private def getDir(param: String, dirName: String) = {
    val path = (settingsJSON \ param).asOpt[String] match {
      case Some(x) if x.length > 0 => Some(x)
      case _ => Some(DefaultDataDir + dirName)
    }
    path.ensuring(pathOpt => pathOpt.forall(directoryEnsuring), s"Can't create or read folder '$path'")
  }
}
