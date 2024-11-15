package com.wavesplatform.settings.utils

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.instances.list.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import com.typesafe.config.{Config, ConfigException, ConfigFactory, ConfigValue}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import pureconfig.*

import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import scala.util.Try

object ConfigSettingsValidator {

  type ErrorsListOr[A] = ValidatedNel[String, A]

  def apply(config: Config): ConfigSettingsValidator = new ConfigSettingsValidator(config)

  implicit class ErrorListOrOps[A](validatedValue: ErrorsListOr[A]) {
    def getValueOrThrowErrors: A = validatedValue valueOr (errorsAcc => throw new Exception(errorsAcc.mkString_(", ")))
  }

  object AdhocValidation {
    def validateAssetPairKey(key: String): Validated[String, AssetPair] =
      Validated.fromTry(AssetPair.fromString(key)) leftMap (_ => s"Can't parse asset pair '$key'")
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

    NonEmptyList.one(s"Invalid setting $settingName value: $msg")
  }

  def validate[T: ConfigReader: ClassTag](settingName: String, showError: Boolean = false): ErrorsListOr[T] = {
    Validated fromTry Try(ConfigSource.fromConfig(config).at(settingName).loadOrThrow[T]) leftMap (ex =>
      createError(settingName, ex.getMessage, showError)
    )
  }

  def validateByPredicate[T: ConfigReader: ClassTag](settingName: String)(predicate: T => Boolean, errorMsg: String): ErrorsListOr[T] = {
    validate[T](settingName, showError = true).ensure(createError(settingName, errorMsg))(predicate)
  }

  def validatePercent(settingName: String): ErrorsListOr[Double] = {
    validateByPredicate[Double](settingName)(p => 0 < p && p <= 100, "required 0 < percent <= 100")
  }

  private def configFromConfigValue(configValue: ConfigValue) =
    ConfigFactory.empty().withValue("stubKey", configValue).getConfig("stubKey")

  def validateList[T: ConfigReader: ClassTag](settingName: String): ErrorsListOr[List[T]] = {
    config
      .getList(settingName)
      .asScala
      .toList
      .zipWithIndex
      .traverse { case (cfg, index) =>
        val elemPath = s"$settingName.$index"
        Validated fromTry Try(ConfigSource.fromConfig(configFromConfigValue(cfg)).at(elemPath).loadOrThrow[T]) leftMap (ex => List(ex.getMessage))
      }
      .leftMap(errorsInList => createError(settingName, errorsInList.mkString(", "), showValue = false))
  }

  def validateMap[K, V: ConfigReader: ClassTag](settingName: String)(keyValidator: String => Validated[String, K]): ErrorsListOr[Map[K, V]] = {
    config
      .getConfig(settingName)
      .root()
      .entrySet()
      .asScala
      .toList
      .traverse { entry =>
        val elemPath = s"$settingName.${entry.getKey}"
        val k        = keyValidator(entry.getKey).leftMap(List(_))
        val v = Validated fromTry Try(ConfigSource.fromConfig(configFromConfigValue(entry.getValue)).at(elemPath).loadOrThrow[V]) leftMap (ex =>
          List(ex.getMessage)
        )
        k.product(v)
      }
      .map(_.toMap)
      .leftMap(errorsInList => createError(settingName, errorsInList.mkString(", "), showValue = false))
  }

  def validateWithDefault[T: ConfigReader: ClassTag](settingName: String, defaultValue: T, showError: Boolean = false): ErrorsListOr[T] = {
    Validated
      .fromTry(Try(ConfigSource.fromConfig(config).at(settingName).loadOrThrow[T]).recover { case _: ConfigException.Missing => defaultValue })
      .leftMap(ex => createError(settingName, ex.getMessage, showError))
  }

  def validateByPredicateWithDefault[T: ConfigReader: ClassTag](
      settingName: String
  )(predicate: T => Boolean, errorMsg: String, defaultValue: T): ErrorsListOr[T] = {
    validateWithDefault[T](settingName, defaultValue, showError = true).ensure(createError(settingName, errorMsg))(predicate)
  }
}
