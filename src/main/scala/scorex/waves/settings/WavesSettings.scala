package scorex.waves.settings

import scorex.settings.Settings
import scorex.transaction.TransactionSettings

class WavesSettings(override val filename: String) extends Settings with TransactionSettings {
  override lazy val genesisTimestamp = 1460678400000L
}
