package com.wavesplatform.settings

import cats.implicits._
import com.wavesplatform.settings.utils.ConfigSettingsValidator
import com.wavesplatform.settings.utils.ConfigSettingsValidator.ErrorsListOr
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

case class DeviationsSettings(enabled: Boolean, maxPriceProfit: Double, maxPriceLoss: Double, maxPriceFee: Double)

object DeviationsSettings {

  implicit val deviationsSettingsReader: ValueReader[DeviationsSettings] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    def validateDeviationPercent(settingName: String): ErrorsListOr[Double] = {
      cfgValidator.validateByPredicate[Double](settingName)(_ > 0, "required 0 < percent")
    }

    (
      cfgValidator.validate[Boolean](s"$path.enable"),
      validateDeviationPercent(s"$path.profit"),
      validateDeviationPercent(s"$path.loss"),
      validateDeviationPercent(s"$path.fee")
    ) mapN DeviationsSettings.apply valueOr (errorsAcc => throw new Exception(errorsAcc.mkString(", ")))
  }
}
