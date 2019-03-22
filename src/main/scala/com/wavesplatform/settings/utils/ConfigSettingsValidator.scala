package com.wavesplatform.settings.utils

import cats.data.Validated
import cats.implicits._
import com.typesafe.config.Config
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

import scala.util.Try

object ConfigSettingsValidator {

  type ErrorsListOr[A] = Validated[List[String], A]

  def apply(config: Config): ConfigSettingsValidator = new ConfigSettingsValidator(config)

  def createInvalidSettingValueError[T](settingName: String, settingValue: T, additionalErrorInfo: String = ""): List[String] = {
    val errorMsg = Option(additionalErrorInfo).filter(_.nonEmpty).fold("")(m => s", $m")
    List(s"Invalid setting $settingName value: $settingValue$errorMsg")
  }
}

class ConfigSettingsValidator(config: Config) {

  import ConfigSettingsValidator._

  def validateByPredicate[T: ValueReader](settingName: String)(predicate: T => Boolean, errorMsg: String): ErrorsListOr[T] = {
    val settingValue = config.as[T](settingName)
    Validated cond (predicate(settingValue), settingValue, createInvalidSettingValueError(settingName, settingValue, errorMsg))
  }

  def validateSafeFromString[T: ValueReader](settingName: String): ErrorsListOr[T] = {
    Validated fromTry Try(config.as[T](settingName)) leftMap (_ => createInvalidSettingValueError(settingName, config getString settingName))
  }

  def validateAsset(settingName: String): ErrorsListOr[Asset] = {
    val assetStr = config getString settingName
    Validated fromTry (AssetPair extractAssetId assetStr) leftMap (_ => createInvalidSettingValueError(settingName, assetStr))
  }

  def validatePercent(settingName: String): ErrorsListOr[Double] = {
    validateByPredicate[Double](settingName)(p => 0 < p && p <= 100, "required 0 < percent <= 100")
  }

  def validateAssetPairSet(settingName: String): ErrorsListOr[Set[AssetPair]] = {

    def validateAssetStrList(list: List[(String, String)]): Either[List[String], Set[AssetPair]] = {
      list
        .traverse {
          case (amountAssetStr, priceAssetStr) =>
            Validated fromTry (AssetPair createAssetPair (amountAssetStr, priceAssetStr)) leftMap (_ => List(s"[$amountAssetStr, $priceAssetStr]"))
        }
        .map(_.toSet)
        .leftMap(invalidPairs => createInvalidSettingValueError(settingName, invalidPairs.mkString(", ")))
        .toEither
    }

    def toTupleList(list: List[List[String]]): List[(String, String)] = list map {
      case amountAsset :: priceAsset :: Nil => amountAsset -> priceAsset
    }

    lazy val rawPairsList = config.as[List[List[String]]](settingName)

    Either
      .fromTry(Try(rawPairsList))
      .leftMap(ex => List(s"Invalid setting $settingName value (${ex.getMessage})"))
      .ensure(
        List(s"Assets count in the pair != 2: ${rawPairsList.withFilter(_.size != 2).map(_.mkString("[", ", ", "]")).mkString(", ")}")
      )(_ forall (_.size == 2))
      .map(toTupleList)
      .flatMap(validateAssetStrList)
      .toValidated
  }
}
