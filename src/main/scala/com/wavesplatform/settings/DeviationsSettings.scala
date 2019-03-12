package com.wavesplatform.settings

import cats.implicits._
import com.wavesplatform.settings.utils.ConfigSettingsValidator
import net.ceedubs.ficus.readers.ValueReader

case class DeviationsSettings(maxPriceProfit: Double, maxPriceLoss: Double, maxPriceDeviationFee: Double)

object DeviationsSettings {

  implicit val deviationsSettingsReader: ValueReader[DeviationsSettings] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)
    (
      cfgValidator.validatePercent(s"$path.max-price-profit"),
      cfgValidator.validatePercent(s"$path.max-price-loss"),
      cfgValidator.validatePercent(s"$path.max-price-fee")
    ) mapN DeviationsSettings.apply valueOr (errorsAcc => throw new Exception(errorsAcc.mkString("\n")))
  }
}
