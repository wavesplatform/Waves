package com.wavesplatform

import java.io.File

import com.typesafe.config.{Config, ConfigFactory, ConfigUtil}
import com.wavesplatform.state2.ByteStr

import collection.JavaConverters._
import net.ceedubs.ficus.readers.namemappers.HyphenNameMapper
import net.ceedubs.ficus.readers.{NameMapper, ValueReader}

package object settings {
  implicit val hyphenCase: NameMapper = HyphenNameMapper

  implicit val fileReader: ValueReader[File]        = (cfg, path) => new File(cfg.getString(path))
  implicit val byteStrReader: ValueReader[ByteStr]  = (cfg, path) => ByteStr.decodeBase58(cfg.getString(path)).get
  implicit val shortValueReader: ValueReader[Short] = (cfg, path) => cfg.getLong(path).toShort
  implicit val mapShortIntReader: ValueReader[Map[Short, Int]] = { (cfg, path) =>
    val relativeConfig = cfg.getConfig(path)
    relativeConfig
      .root()
      .entrySet()
      .asScala
      .map { entry =>
        val key = entry.getKey
        key.toShort -> relativeConfig.getInt(ConfigUtil.quoteString(key))
      }
      .toMap
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
