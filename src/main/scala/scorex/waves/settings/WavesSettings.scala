package scorex.waves.settings

import scorex.settings.Settings
import scorex.transaction.TransactionSettings

import scala.util.Try

class WavesSettings(override val filename: String) extends Settings with TransactionSettings {
  override lazy val genesisTimestamp = 1465742577614L
  override val MaxBlocksChunks = 20

  val suspendedSenders = Try {
    (settingsJSON \ "suspendedSenders").as[List[String]]
  }.getOrElse(List[String]())
}
