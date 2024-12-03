package com.wavesplatform.settings

import pureconfig.*
import pureconfig.generic.derivation.default.*

case class FeaturesSettings(autoShutdownOnUnsupportedFeature: Boolean, supported: List[Short] = List.empty) derives ConfigReader
