package scorex.lagonaki.settings

import scorex.perma.settings.PermaSettings
import scorex.settings.Settings
import scorex.transaction.TransactionSettings

class LagonakiSettings(override val filename: String) extends Settings with TransactionSettings with PermaSettings