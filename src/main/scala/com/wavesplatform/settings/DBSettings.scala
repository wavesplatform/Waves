package com.wavesplatform.settings
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.{ArbitraryTypeReader, ValueReader}

case class DBSettings(directory: String, storeTransactionsByAddress: Boolean)

object DBSettings {
  implicit val dbSettingsValueReader: ValueReader[DBSettings] = ArbitraryTypeReader.arbitraryTypeValueReader
}
