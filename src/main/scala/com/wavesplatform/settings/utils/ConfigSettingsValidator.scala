package com.wavesplatform.settings.utils

import cats.data.Validated
import cats.implicits._
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

import scala.collection.JavaConverters._
import scala.util.Try

object ConfigSettingsValidator {
  type ErrorsListOr[A] = Validated[List[String], A]
  def apply(config: Config): ConfigSettingsValidator = new ConfigSettingsValidator(config)
}

class ConfigSettingsValidator(config: Config) {

  import ConfigSettingsValidator.ErrorsListOr

  private def createError[T](settingName: String, additionalErrorInfo: String = ""): List[String] = {
    val errorMsg = Option(additionalErrorInfo).filter(_.nonEmpty).fold("")(m => s", $m")
    List(s"Invalid setting $settingName value: ${config.getValue(settingName).unwrapped}$errorMsg")
  }

  def validate[T: ValueReader](settingName: String, f: Throwable => String = _ => ""): ErrorsListOr[T] = {
    Validated fromTry Try(config.as[T](settingName)) leftMap (ex => createError(settingName, f(ex)))
  }

  def validateByPredicate[T: ValueReader](settingName: String)(predicate: T => Boolean, errorMsg: String): ErrorsListOr[T] = {
    validate[T](settingName, _.getMessage).ensure(createError(settingName, errorMsg))(predicate)
  }

  def validatePercent(settingName: String): ErrorsListOr[Double] = {
    validateByPredicate[Double](settingName)(p => 0 < p && p <= 100, "required 0 < percent <= 100")
  }

  def validateList[T: ValueReader](settingName: String): ErrorsListOr[List[T]] = {
    config
      .getList(settingName)
      .asScala
      .toList
      .zipWithIndex
      .traverse {
        case (cfg, index) =>
          val elemPath = s"$settingName.$index"
          Validated fromTry Try(cfg.atPath(elemPath).as[T](elemPath)) leftMap (ex => List(ex.getMessage))
      }
      .leftMap(errorsInList => createError(settingName, errorsInList.mkString(", ")))
  }
}
