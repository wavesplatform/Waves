package com.wavesplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

case class FeaturesSettings(autoActivate: Boolean,
                            autoStop: Boolean,
                            supported: List[Int])

object FeaturesSettings {
  val configPath: String = "waves.features"

  def fromConfig(config: Config): FeaturesSettings = {
    val autoActivate = config.as[Boolean](s"$configPath.auto-activate")
    val autoStop = config.as[Boolean](s"$configPath.auto-stop")
    val supported = config.as[List[Int]](s"$configPath.supported")

    FeaturesSettings(autoActivate, autoStop, supported)
  }
}
