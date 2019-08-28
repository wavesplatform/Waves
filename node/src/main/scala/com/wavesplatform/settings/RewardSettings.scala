package com.wavesplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

case class RewardSettings(supported: Byte)

object RewardSettings {
  implicit val valueReader: ValueReader[RewardSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  private[this] def fromConfig(config: Config): RewardSettings = {
    implicit val byteValueReader: ValueReader[Byte] = (cfg, path) => cfg.getInt(path).toByte

    RewardSettings(config.getOrElse("supported", 0.toByte))
  }
}