package com.wavesplatform.settings
import com.wavesplatform.matcher.MatcherSettings

trait CustomValueReaders {
  implicit val networkSettingsValueReader    = NetworkSettings.valueReader
  implicit val blockchainSettingsValueReader = BlockchainSettings.valueReader
  implicit val matcherSettingsValueReader    = MatcherSettings.valueReader
}
