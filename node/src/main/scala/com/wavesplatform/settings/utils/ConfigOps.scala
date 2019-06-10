package com.wavesplatform.settings.utils

import cats.data.Validated
import com.typesafe.config.Config
import com.wavesplatform.settings.utils.ConfigSettingsValidator.ErrorListOrOps
import net.ceedubs.ficus.readers.ValueReader

object ConfigOps {

  implicit class ConfigOps(config: Config) {

    val cfgValidator = ConfigSettingsValidator(config)

    def getValidatedSet[T: ValueReader](path: String): Set[T] = {
      cfgValidator.validateList[T](path).map(_.toSet) getValueOrThrowErrors
    }

    def getValidatedMap[K, V: ValueReader](path: String)(keyValidator: String => Validated[String, K]): Map[K, V] = {
      cfgValidator.validateMap(path)(keyValidator) getValueOrThrowErrors
    }

    def getValidatedByPredicate[T: ValueReader](path: String)(predicate: T => Boolean, errorMsg: String): T = {
      cfgValidator.validateByPredicate(path)(predicate, errorMsg) getValueOrThrowErrors
    }
  }
}
