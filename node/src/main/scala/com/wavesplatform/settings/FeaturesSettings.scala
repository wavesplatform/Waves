package com.wavesplatform.settings

import pureconfig.*

case class FeaturesSettings(autoShutdownOnUnsupportedFeature: Boolean, supported: List[Short] = defaultSupported)

object FeaturesSettings {
  // Note: This setup (default values + manual ConfigReader instance) 
  // is a workaround for `pureconfig-generic-scala3` (it doesn't support default values from case classes yet)
  val defaultSupported: List[Short] = List.empty

  given ConfigReader[FeaturesSettings] = ConfigReader.fromCursor(cur =>
    for {
      objCur <- cur.asObjectCursor
      autoShutdownOnUnsupportedFeature <- objCur.required[Boolean]("auto-shutdown-on-unsupported-feature")
      supported <- objCur.optionalWithDefault("supported", defaultSupported)
    } yield FeaturesSettings(autoShutdownOnUnsupportedFeature, supported)
  )
}
