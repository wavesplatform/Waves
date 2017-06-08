package com.wavesplatform

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.state2.ByteStr
import net.ceedubs.ficus.readers.ValueReader

package object settings {
  implicit val optionalPathValueReader: ValueReader[Option[File]] =
    (config: Config, path: String) => config.getString(path).trim match {
      case "" => None
      case str => Some(new File(str))
    }

  implicit val optionalByteStrReader: ValueReader[Option[ByteStr]] =
    (config: Config, path: String) => config.getString(path).trim match {
      case "" => None
      case str => Some(ByteStr.decodeBase58(str).get)
    }


  def loadConfig(userConfig: Config): Config = {
    ConfigFactory
      .defaultOverrides()
      .withFallback(userConfig)
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()
  }
}
