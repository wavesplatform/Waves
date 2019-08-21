package com.wavesplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

case class FeaturesSettings(autoShutdownOnUnsupportedFeature: Boolean, supported: List[Short], reward: Byte)

object FeaturesSettings {
  implicit val valueReader: ValueReader[FeaturesSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  private[this] def fromConfig(config: Config): FeaturesSettings = {
    implicit val byteValueReader: ValueReader[Byte] = (cfg, path) => cfg.getInt(path).toByte

    val autoShutdownOnUnsupportedFeature = config.as[Boolean]("auto-shutdown-on-unsupported-feature")
    val supportedFeatures                = config.as[List[Short]]("supported")
    val rewardVote                       = config.getOrElse("reward", 0.toByte)

    FeaturesSettings(autoShutdownOnUnsupportedFeature, supportedFeatures, rewardVote)
  }
}
