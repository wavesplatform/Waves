package scorex.lagonaki.server

import scorex.settings.Settings
import scorex.transaction.TransactionSettings

class LagonakiSettings(override val filename: String) extends Settings with TransactionSettings
