package com.wavesplatform.settings.utils

import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.typesafe.config.Config
import net.ceedubs.ficus.readers.ValueReader

object ConfigOps {

  implicit class ConfigOps(config: Config) {

    val cfgValidator = ConfigSettingsValidator(config)

    private def throwErrors(list: NonEmptyList[String]) = throw new Exception(list.mkString_(", "))

    def getValidatedSet[T: ValueReader](path: String): Set[T] = {
      cfgValidator.validateList[T](path).map(_.toSet) valueOr throwErrors
    }

    def getValidatedMap[K, V: ValueReader](path: String)(keyReader: String => Validated[String, K]): Map[K, V] = {
      cfgValidator.validateMap(path)(keyReader) valueOr throwErrors
    }

    def getValidatedByPredicate[T: ValueReader](path: String)(predicate: T => Boolean, errorMsg: String): T = {
      cfgValidator.validateByPredicate(path)(predicate, errorMsg) valueOr throwErrors
    }
  }
}
