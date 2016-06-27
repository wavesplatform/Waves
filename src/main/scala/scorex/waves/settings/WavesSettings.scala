package scorex.waves.settings

import scorex.settings.Settings
import scorex.transaction.TransactionSettings

import scala.util.Try

class WavesSettings(override val filename: String) extends Settings with TransactionSettings {
  override lazy val genesisTimestamp = 1460678400000L
  override val MaxBlocksChunks = 20

  // TODO: Should be moved to Scorex TransactionSettings
  lazy val minimumTxFee = (settingsJSON \ "minimumTxFee").asOpt[Int].getOrElse(DefaultMinimumTxFee)
  private val DefaultMinimumTxFee = 1000

  val suspendedSenders = Try {
    (settingsJSON \ "suspendedSenders").as[List[String]]
  }.getOrElse(List[String]())
}
