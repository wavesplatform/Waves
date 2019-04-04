package com.wavesplatform.settings
import net.ceedubs.ficus.readers.{ArbitraryTypeReader, ValueReader}
import net.ceedubs.ficus.Ficus._

case class UtxSettings(maxSize: Int,
                       maxBytesSize: Long,
                       maxScriptedSize: Int,
                       blacklistSenderAddresses: Set[String],
                       allowBlacklistedTransferTo: Set[String],
                       allowTransactionsFromSmartAccounts: Boolean,
                       allowSkipChecks: Boolean)

object UtxSettings {
  implicit val utxSettingsValueReader: ValueReader[UtxSettings] = ArbitraryTypeReader.arbitraryTypeValueReader
}
