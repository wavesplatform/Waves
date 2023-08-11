package com.wavesplatform.ride.runner.entrypoints

import com.typesafe.config.*
import com.wavesplatform.account.Address
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.{CollectionReaders, ValueReader}
import play.api.libs.json.{JsObject, Json}

package object settings {
  implicit val configMemorySizeValueReader: ValueReader[ConfigMemorySize] = (config: Config, path: String) => config.getMemorySize(path)

  implicit val testRequestsValueReader: ValueReader[List[(Address, JsObject)]] = CollectionReaders
    .traversableReader[List, ConfigValue]
    .map { xs =>
      xs.map {
        case xs: ConfigList if xs.unwrapped().size() == 2 =>
          val key = xs.get(0).unwrapped() match {
            case k: String =>
              Address.fromString(k) match {
                case Right(x) => x
                case Left(e)  => throw new RuntimeException(s"Can't parse '$k' as Address: ${e.toString}")
              }
            case k => throw new RuntimeException(s"Can't parse as Address: $k")
          }

          val strV = xs.get(1).render(ConfigRenderOptions.concise())
          val value = Json.parse(strV) match {
            case x: JsObject => x
            case x           => throw new RuntimeException(s"Can't parse value as JsObject: $x")
          }

          key -> value

        case xs => throw new RuntimeException(s"Expected two elements, got: $xs")
      }
    }
}
