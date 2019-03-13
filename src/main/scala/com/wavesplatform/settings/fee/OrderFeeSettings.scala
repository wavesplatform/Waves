package com.wavesplatform.settings.fee

import cats.data.Validated
import cats.implicits._
import com.wavesplatform.settings.fee.AssetType.AssetType
import com.wavesplatform.settings.fee.Mode.Mode
import com.wavesplatform.settings.utils.ConfigSettingsValidator
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.EnumerationReader._
import net.ceedubs.ficus.readers.ValueReader

object OrderFeeSettings {

  type ErrorsListOr[A] = Validated[List[String], A]

  sealed trait OrderFeeSettings

  case class FixedWavesSettings(minFee: Long)                      extends OrderFeeSettings
  case class FixedSettings(defaultAssetId: Asset, minFee: Long)    extends OrderFeeSettings
  case class PercentSettings(assetType: AssetType, minFee: Double) extends OrderFeeSettings

  implicit val orderFeeSettingsReader: ValueReader[OrderFeeSettings] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    def getPrefixByMode(mode: Mode): String = s"$path.$mode"

    def validateNonWavesAsset(settingName: String)(assetIdStr: String): ErrorsListOr[Asset] = {
      cfgValidator
        .validateAsset(settingName)(assetIdStr)
        .ensure(List(s"Invalid setting $settingName value: $assetIdStr, asset must not be Waves"))(_ != Waves)
    }

    def validateFixedWavesSettings: ErrorsListOr[FixedWavesSettings] = {
      cfgValidator.validateByPredicate[Long](s"${getPrefixByMode(Mode.FIXED_WAVES)}.min-fee")(_ > 0, "must be > 0") map FixedWavesSettings
    }

    def validateFixedSettings: ErrorsListOr[FixedSettings] = {

      val prefix                 = getPrefixByMode(Mode.FIXED)
      val assetSettingName       = s"$prefix.asset-id"
      val fixedMinFeeSettingName = s"$prefix.min-fee"

      val assetStr = cfg.getString(assetSettingName)

      (
        validateNonWavesAsset(assetSettingName)(assetStr),
        cfgValidator.validateByPredicate[Long](fixedMinFeeSettingName)(_ > 0, "must be > 0")
      ) mapN FixedSettings
    }

    def validatePercentSettings: ErrorsListOr[PercentSettings] = {

      val prefix             = getPrefixByMode(Mode.PERCENT)
      val percentSettingName = s"$prefix.min-fee"

      (
        cfgValidator.validateSafe[AssetType](s"$prefix.asset-type"),
        cfgValidator.validatePercent(percentSettingName)
      ) mapN PercentSettings
    }

    def getSettingsByMode(mode: Mode): ErrorsListOr[OrderFeeSettings] = mode match {
      case Mode.FIXED_WAVES => validateFixedWavesSettings
      case Mode.FIXED       => validateFixedSettings
      case Mode.PERCENT     => validatePercentSettings
    }

    cfgValidator.validateSafe[Mode](s"$path.mode").toEither.flatMap(mode => getSettingsByMode(mode).toEither) match {
      case Left(errorsAcc)         => throw new Exception(errorsAcc.mkString("\n"))
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

  val FIXED_WAVES = Value("fixed-waves")
  val FIXED       = Value("fixed")
  val PERCENT     = Value("percent")
}
