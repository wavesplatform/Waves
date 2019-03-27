package com.wavesplatform.settings

import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.{ArbitraryTypeReader, ValueReader}

import scala.concurrent.duration.FiniteDuration

case class UtxSettings(maxSize: Int,
                       maxBytesSize: Long,
                       blacklistSenderAddresses: Set[String],
                       allowBlacklistedTransferTo: Set[String],
                       cleanupInterval: FiniteDuration,
                       allowTransactionsFromSmartAccounts: Boolean)

object UtxSettings {
  implicit val utxSettingsValueReader: ValueReader[UtxSettings] = ArbitraryTypeReader.arbitraryTypeValueReader
}
