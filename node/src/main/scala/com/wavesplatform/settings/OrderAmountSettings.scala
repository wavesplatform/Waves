package com.wavesplatform.settings

import cats.implicits._
import com.wavesplatform.settings.utils.ConfigSettingsValidator
import com.wavesplatform.settings.utils.ConfigSettingsValidator.ErrorsListOr
import com.wavesplatform.transaction.assets.exchange.AssetPair
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

case class OrderAmountSettings(stepSize: Double, minAmount: Double, maxAmount: Double)

object OrderAmountSettings {

  implicit val orderAmountSettingsReader: ValueReader[(AssetPair, OrderAmountSettings)] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    def validateAmounts(stepSize: Double): ErrorsListOr[(Double, Double)] = {

      def amtValidator(settingName: String): ErrorsListOr[Double] = {
        cfgValidator.validateByPredicate[Double](settingName)(
          amt => 0 < BigDecimal.valueOf(amt) && BigDecimal.valueOf(amt).remainder(stepSize) == 0,
          s"amount must be a multiple of step-size ($stepSize) and must be > 0"
        )
      }

      (amtValidator(s"$path.min-amount"), amtValidator(s"$path.max-amount"))
        .mapN(Tuple2.apply)
        .ensure(List("required min-amount < max-amount")) { case (minAmt, maxAmt) => minAmt < maxAmt }
    }

    lazy val assetPairValidated = cfgValidator.validate[AssetPair](s"$path.pair")
    lazy val stepSizeValidated  = cfgValidator.validateByPredicate[Double](s"$path.step-size")(_ > 0, "required 0 < step size")

    (assetPairValidated, stepSizeValidated)
      .mapN(Tuple2.apply)
      .toEither
      .flatMap {
        case (assetPair, stepSize) => validateAmounts(stepSize).toEither.map { case (minAmt, maxAmt) => (assetPair, stepSize, minAmt, maxAmt) }
      } match {
      case Left(errorsAcc)                                    => throw new Exception(errorsAcc.mkString(", "))
      case Right((assetPair, stepSize, minAmount, maxAmount)) => assetPair -> OrderAmountSettings(stepSize, minAmount, maxAmount)
    }
  }
}
