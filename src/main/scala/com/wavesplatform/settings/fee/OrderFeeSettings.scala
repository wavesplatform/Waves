package com.wavesplatform.settings.fee

import cats.data.Validated.Valid
import cats.implicits._
import com.wavesplatform.settings.Constants
import com.wavesplatform.settings.fee.AssetType.AssetType
import com.wavesplatform.settings.fee.Mode.Mode
import com.wavesplatform.settings.utils.ConfigSettingsValidator
import com.wavesplatform.settings.utils.ConfigSettingsValidator.ErrorsListOr
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.AssetPair
import monix.eval.Coeval
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.EnumerationReader._
import net.ceedubs.ficus.readers.ValueReader
import play.api.libs.json.{JsObject, Json}

object OrderFeeSettings {

  val totalWavesAmount: Long = Constants.UnitsInWave * Constants.TotalWaves

  sealed trait OrderFeeSettings {

    /** Returns json for order fee settings taking into account fee that should be paid for matcher's account script invocation */
    def getJson(matcherAccountFee: Long): Coeval[JsObject] = Coeval.evalOnce {
      Json.obj(
        this match {
          case FixedWavesSettings(baseFee) =>
            "waves" -> Json.obj(
              "baseFee" -> (baseFee + matcherAccountFee)
            )
          case FixedSettings(defaultAssetId, minFee) =>
            "fixed" -> Json.obj(
              "assetId" -> AssetPair.assetIdStr(defaultAssetId),
              "minFee"  -> minFee
            )
          case PercentSettings(assetType, minFee) =>
            "percent" -> Json.obj(
              "type"   -> assetType,
              "minFee" -> minFee
            )
        }
      )
    }
  }

  case class FixedWavesSettings(baseFee: Long)                     extends OrderFeeSettings
  case class FixedSettings(defaultAssetId: Asset, minFee: Long)    extends OrderFeeSettings
  case class PercentSettings(assetType: AssetType, minFee: Double) extends OrderFeeSettings

  implicit val orderFeeSettingsReader: ValueReader[OrderFeeSettings] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    def getPrefixByMode(mode: Mode): String = s"$path.$mode"

    def validateWavesSettings: ErrorsListOr[FixedWavesSettings] = {
      cfgValidator.validateByPredicate[Long](s"${getPrefixByMode(Mode.WAVES)}.base-fee")(
        predicate = fee => 0 < fee && fee <= totalWavesAmount,
        errorMsg = s"required 0 < base fee <= $totalWavesAmount"
      ) map FixedWavesSettings
    }

    def validateFixedSettings: ErrorsListOr[FixedSettings] = {

      val prefix         = getPrefixByMode(Mode.FIXED)
      val feeValidator   = cfgValidator.validateByPredicate[Long](s"$prefix.min-fee") _
      val assetValidated = cfgValidator.validateAsset(s"$prefix.asset")

      val feeValidated = assetValidated match {
        case Valid(Waves) => feeValidator(fee => 0 < fee && fee <= totalWavesAmount, s"required 0 < fee <= $totalWavesAmount")
        case _            => feeValidator(_ > 0, "required 0 < fee")
      }

      (assetValidated, feeValidated) mapN FixedSettings
    }

    def validatePercentSettings: ErrorsListOr[PercentSettings] = {
      val prefix = getPrefixByMode(Mode.PERCENT)
      (
        cfgValidator.validateSafe[AssetType](s"$prefix.asset-type"),
        cfgValidator.validatePercent(s"$prefix.min-fee")
      ) mapN PercentSettings
    }

    def getSettingsByMode(mode: Mode): ErrorsListOr[OrderFeeSettings] = mode match {
      case Mode.WAVES   => validateWavesSettings
      case Mode.FIXED   => validateFixedSettings
      case Mode.PERCENT => validatePercentSettings
    }

    cfgValidator.validateSafe[Mode](s"$path.mode").toEither flatMap (mode => getSettingsByMode(mode).toEither) match {
      case Left(errorsAcc)         => throw new Exception(errorsAcc.mkString(", "))
      case Right(orderFeeSettings) => orderFeeSettings
    }
  }
}

object AssetType extends Enumeration {
  type AssetType = Value

  val AMOUNT    = Value("amount")
  val PRICE     = Value("price")
  val SPENDING  = Value("spending")
  val RECEIVING = Value("receiving")
}

object Mode extends Enumeration {
  type Mode = Value

  val WAVES   = Value("waves")
  val FIXED   = Value("fixed")
  val PERCENT = Value("percent")
}
