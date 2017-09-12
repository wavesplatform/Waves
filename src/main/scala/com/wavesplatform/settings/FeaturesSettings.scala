package com.wavesplatform.settings

case class FeaturesSettings(autoActivate: Boolean,
                            autoShutdownOnUnsupportedFeature: Boolean,
                            supported: List[Int])
