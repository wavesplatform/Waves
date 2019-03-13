package com.wavesplatform.settings.utils

import cats.data.Validated
import com.typesafe.config.Config
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

import scala.util.Try

object ConfigSettingsValidator {
  type ErrorsListOr[A] = Validated[List[String], A]
  def apply(config: Config): ConfigSettingsValidator = new ConfigSettingsValidator(config)
}

class ConfigSettingsValidator(config: Config) {

  import ConfigSettingsValidator._

  def validateByPredicate[T: ValueReader](settingName: String)(predicate: T => Boolean, errorMsg: String): ErrorsListOr[T] = {

    val settingValue   = config.as[T](settingName)
    val additionalInfo = Option(errorMsg).filter(_.nonEmpty).fold("")(m => s", $m")

    Validated.cond(predicate(settingValue), settingValue, List(s"Invalid setting $settingName value: $settingValue$additionalInfo"))
  }

  def validateSafe[T: ValueReader](settingName: String): ErrorsListOr[T] = {
    Validated.fromTry(Try(config.as[T](settingName))).leftMap(_ => List(s"Invalid setting $settingName value: ${config.getString(settingName)}"))
  }

  def validateAsset(settingName: String)(assetIdStr: String): ErrorsListOr[Asset] = {
    Validated
      .fromTry(AssetPair.extractAssetId(assetIdStr))
      .leftMap(_ => List(s"Invalid setting $settingName value: $assetIdStr"))
  }

  def validatePercent(settingName: String): ErrorsListOr[Double] = {
    validateByPredicate[Double](settingName)(p => 0 < p && p <= 100, "required 0 < p <= 100")
  }
}
