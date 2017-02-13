package com.wavesplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

case class CheckpointsSettings(publicKey: String)

object CheckpointsSettings {
  val configPath: String = "waves.checkpoints"

  def fromConfig(config: Config): CheckpointsSettings = {
    val publicKey = config.as[String](s"$configPath.public-key")

    CheckpointsSettings(publicKey)
  }
}