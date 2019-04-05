package com.wavesplatform.settings

import cats.implicits._
import com.wavesplatform.settings.utils.ConfigSettingsValidator
import com.wavesplatform.settings.utils.ConfigSettingsValidator.ErrorsListOr
import com.wavesplatform.transaction.assets.exchange.AssetPair
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

case class OrderRestrictionsSettings(stepSize: Double,
                                     minAmount: Double,
                                     maxAmount: Double,
                                     tickSize: Double,
                                     minPrice: Double,
                                     maxPrice: Double,
                                     mergeSmallPrices: Boolean)

object OrderRestrictionsSettings {

  val stepSizeDefault, tickSizeDefault, minAmountDefault, minPriceDefault = 0.00000001
  val maxAmountDefault                                                    = 1000000000
  val maxPriceDefault                                                     = 1000000

  implicit val orderRestrictionsSettingsReader: ValueReader[(AssetPair, OrderRestrictionsSettings)] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    def validateSizeMinMax(sizeSettingName: String,
                           minSettingName: String,
                           maxSettingName: String,
                           sizeDefaultValue: Double,
                           minDefaultValue: Double,
                           maxDefaultValue: Double): ErrorsListOr[(Double, Double, Double)] = {

      def validateSetting(settingName: String, defaultValue: Double): ErrorsListOr[Double] =
        cfgValidator.validateByPredicateWithDefault[Double](settingName)(_ > 0, s"required 0 < value", defaultValue)

      (
        validateSetting(sizeSettingName, sizeDefaultValue),
        validateSetting(minSettingName, minDefaultValue),
        validateSetting(maxSettingName, maxDefaultValue)
      ).mapN(Tuple3.apply)
        .ensure(List(s"Required $minSettingName < $maxSettingName")) { case (_, min, max) => min < max }
    }

    lazy val validateAssetPair        = cfgValidator.validate[AssetPair](s"$path.pair")
    lazy val validateMergeSmallPrices = cfgValidator.validateWithDefault(s"$path.merge-small-prices", false)

    (
      validateAssetPair,
      validateSizeMinMax(s"$path.step-size", s"$path.min-amount", s"$path.max-amount", stepSizeDefault, minAmountDefault, maxAmountDefault),
      validateSizeMinMax(s"$path.tick-size", s"$path.min-price", s"$path.max-price", tickSizeDefault, minPriceDefault, maxPriceDefault),
      validateMergeSmallPrices
    ).mapN {
        case (assetPair, (stepSize, minAmount, maxAmount), (tickSize, minPrice, maxPrice), mergeSmallPrices) =>
          assetPair -> OrderRestrictionsSettings(stepSize, minAmount, maxAmount, tickSize, minPrice, maxPrice, mergeSmallPrices)
      }
      .valueOr(errorsAcc => throw new Exception(errorsAcc.mkString(", ")))
  }
}
