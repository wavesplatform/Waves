package com.wavesplatform.settings
import net.ceedubs.ficus.Ficus._

case class DBSettings(directory: String, storeTransactionsByAddress: Boolean)
