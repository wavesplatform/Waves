package com.wavesplatform.settings.utils

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import com.typesafe.config.{Config, ConfigException}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

import scala.collection.JavaConverters._
import scala.util.Try

object ConfigSettingsValidator {
  type ErrorsListOr[A] = ValidatedNel[String, A]
  def apply(config: Config): ConfigSettingsValidator = new ConfigSettingsValidator(config)

  implicit class ErrorListOrOpts[A](validatedValue: ErrorsListOr[A]) {
    def getValueOrThrowErrors: A = validatedValue valueOr (errorsAcc => throw new Exception(errorsAcc.mkString_(", ")))
  }
}

class ConfigSettingsValidator(config: Config) {

  import ConfigSettingsValidator.ErrorsListOr

  private def createError[T](settingName: String, errorMsg: String, showError: Boolean = true, showValue: Boolean = true): NonEmptyList[String] = {
    lazy val value = config.getValue(settingName).unwrapped
    lazy val msg = (showValue, showError) match {
      case (true, true)  => s"$value ($errorMsg)"
      case (true, false) => s"$value"
      case (false, true) => s"$errorMsg"
      case _             => ""
    }

    NonEmptyList(s"Invalid setting $settingName value: $msg", Nil)
  }

  def validate[T: ValueReader](settingName: String, showError: Boolean = false): ErrorsListOr[T] = {
    Validated fromTry Try(config.as[T](settingName)) leftMap (ex => createError(settingName, ex.getMessage, showError))
  }

  def validateByPredicate[T: ValueReader](settingName: String)(predicate: T => Boolean, errorMsg: String): ErrorsListOr[T] = {
    validate[T](settingName, showError = true).ensure(createError(settingName, errorMsg))(predicate)
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
      .leftMap(errorsInList => createError(settingName, errorsInList.mkString(", "), showValue = false))
  }

  def validateWithDefault[T: ValueReader](settingName: String, defaultValue: T, showError: Boolean = false): ErrorsListOr[T] = {
    Validated
      .fromTry(Try(config.as[T](settingName)).recover { case _: ConfigException.Missing => defaultValue })
      .leftMap(ex => createError(settingName, ex.getMessage, showError))
  }

  def validateByPredicateWithDefault[T: ValueReader](
      settingName: String)(predicate: T => Boolean, errorMsg: String, defaultValue: T): ErrorsListOr[T] = {
    validateWithDefault[T](settingName, defaultValue, showError = true).ensure(createError(settingName, errorMsg))(predicate)
  }
}
