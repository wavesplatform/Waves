package com.wavesplatform.settings

import pureconfig.*
import pureconfig.generic.semiauto.deriveReader

case class FeaturesSettings(autoShutdownOnUnsupportedFeature: Boolean, supported: List[Short] = List.empty)

object FeaturesSettings {
  // This given is required for default args to work.
  // Details: https://github.com/pureconfig/pureconfig/issues/1673
  // Note: the proposed approach with `extension` doesn't work.
  given ConfigReader[FeaturesSettings] = deriveReader
}
