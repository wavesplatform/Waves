package com.wavesplatform.ride.runner.input

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.account.Address
import com.wavesplatform.json.JsonManipulations
import play.api.libs.json.{JsError, JsObject, JsSuccess, JsValue, Json, Reads}
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

object PureconfigImplicits {

  implicit val jsObjectConfigReader: ConfigReader[JsObject] = playJsonConfigReader

  implicit val jsValueConfigReader: ConfigReader[JsValue] = playJsonConfigReader

  private def playJsonConfigReader[T: Reads]: ConfigReader[T] = ConfigReader.fromCursor { cur =>
    for {
      configValue <- cur.asConfigValue
    } yield {
      val stubKey = "stubKey"
      val config  = ConfigFactory.empty().withValue(stubKey, configValue)
      val jsonStr = config.root().render(ConfigRenderOptions.concise())
      JsonManipulations
        .pick(Json.parse(jsonStr), stubKey)
        .getOrElse(fail(s"Expected a value"))
        .validate[T] match {
        case JsSuccess(value, _) => value
        case JsError(errors)     => fail(s"Can't parse: ${errors.mkString("\n")}")
      }
    }
  }

  implicit val addressConfigReader: ConfigReader[Address] =
    ConfigReader.fromString(s => Address.fromString(s).left.map(_ => CannotConvert(s, "Address", "invalid address")))

  private def fail(message: String, cause: Throwable = null) = throw new IllegalArgumentException(message, cause)
}
