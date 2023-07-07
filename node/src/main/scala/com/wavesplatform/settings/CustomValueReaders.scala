package com.wavesplatform.settings

import net.ceedubs.ficus.readers.ValueReader

trait CustomValueReaders {
  implicit val networkSettingsValueReader: ValueReader[NetworkSettings]       = NetworkSettings.valueReader
  implicit val blockchainSettingsValueReader: ValueReader[BlockchainSettings] = BlockchainSettings.valueReader
}
