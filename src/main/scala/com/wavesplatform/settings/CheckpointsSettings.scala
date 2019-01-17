package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import net.ceedubs.ficus.Ficus._

case class CheckpointsSettings(publicKey: ByteStr)

object CheckpointsSettings {
  val configPath: String = "waves.checkpoints"

  def fromConfig(config: Config): CheckpointsSettings = {
    val publicKey = config.as[ByteStr](s"$configPath.public-key")

    CheckpointsSettings(publicKey)
  }
}
