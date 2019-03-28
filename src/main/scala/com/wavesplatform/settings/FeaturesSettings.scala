package com.wavesplatform.settings
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.{ArbitraryTypeReader, ValueReader}

case class FeaturesSettings(autoShutdownOnUnsupportedFeature: Boolean, supported: List[Short])

object FeaturesSettings {
  implicit val featuresSettingsValueReader: ValueReader[FeaturesSettings] = ArbitraryTypeReader.arbitraryTypeValueReader
}
