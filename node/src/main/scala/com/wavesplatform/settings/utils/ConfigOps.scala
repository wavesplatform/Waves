package com.wavesplatform.settings.utils

import com.typesafe.config.Config
import net.ceedubs.ficus.readers.ValueReader

object ConfigOps {

  implicit class ConfigOps(config: Config) {

    val cfgValidator = ConfigSettingsValidator(config)

    def getFailSlowSetOf[T: ValueReader](path: String): Set[T] = {
      cfgValidator.validateList[T](path).map(_.toSet).valueOr(e => throw new Exception(e.mkString(", ")))
    }
  }
}
