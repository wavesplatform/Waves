package com.wavesplatform.matcher.settings

import cats.implicits._
import com.wavesplatform.settings.utils.ConfigSettingsValidator
import com.wavesplatform.settings.utils.ConfigSettingsValidator._
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

case class OrderHistorySettings(ordersBatchLingerMs: Long, ordersBatchEntries: Long, eventsBatchLingerMs: Long, eventsBatchEntries: Long)

object OrderHistorySettings {

  val defaultBatchLingerMs = 1000
  val defaultBatchEntries  = 10000

  implicit val orderHistorySettingsReader: ValueReader[Option[OrderHistorySettings]] = { (cfg, path) =>
    val cfgValidator = ConfigSettingsValidator(cfg)

    def validateBatchSettings(settingName: String, defaultValue: Long): ErrorsListOr[Long] =
      cfgValidator.validateByPredicateWithDefault(s"$path.$settingName")(_ > 0, s"required 0 < ${settingName.replace("-", " ")}", defaultValue)

    if (cfgValidator.validateWithDefault(s"$path.enabled", false) getValueOrThrowErrors) {
      Some(
        (
          validateBatchSettings("orders-batch-linger-ms", defaultBatchLingerMs),
          validateBatchSettings("orders-batch-entries", defaultBatchEntries),
          validateBatchSettings("events-batch-linger-ms", defaultBatchLingerMs),
          validateBatchSettings("events-batch-entries", defaultBatchEntries),
        ) mapN OrderHistorySettings.apply getValueOrThrowErrors
      )
    } else None
  }
}
