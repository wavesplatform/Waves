package com.wavesplatform

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.readers.ValueReader

package object settings {
  implicit val optionalPathValueReader: ValueReader[Option[File]] =
    (config: Config, path: String) => config.getString(path).trim match {
      case "" => None
      case nonEmptyPath => Some(new File(nonEmptyPath))
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
