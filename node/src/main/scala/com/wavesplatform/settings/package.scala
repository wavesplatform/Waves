package com.wavesplatform

import java.io.File
import java.net.{InetSocketAddress, URI}

import com.typesafe.config.{Config, ConfigException, ConfigFactory, ConfigValueType}
import com.wavesplatform.common.state.ByteStr
import net.ceedubs.ficus.readers.namemappers.HyphenNameMapper
import net.ceedubs.ficus.readers.{NameMapper, ValueReader}
import org.apache.commons.lang3.SystemUtils

import scala.collection.JavaConverters._

package object settings {
  implicit val hyphenCase: NameMapper = HyphenNameMapper

  implicit val fileReader: ValueReader[File]        = (cfg, path) => new File(cfg.getString(path))
  implicit val byteStrReader: ValueReader[ByteStr]  = (cfg, path) => ByteStr.decodeBase58(cfg.getString(path)).get
  implicit val shortValueReader: ValueReader[Short] = (cfg, path) => cfg.getLong(path).toShort
  implicit val preactivatedFeaturesReader: ValueReader[Map[Short, Int]] = (config: Config, path: String) =>
    if (config.getIsNull(path)) Map.empty
    else {
      config.getValue(path).valueType() match {
        case ConfigValueType.OBJECT =>
          val paf = config.getConfig(path)
          (for {
            featureId <- paf.root().keySet().asScala
          } yield featureId.toShort -> paf.getInt(featureId)).toMap
        case ConfigValueType.STRING if config.getString(path).isEmpty =>
          Map.empty
        case other =>
          throw new ConfigException.WrongType(config.getValue(path).origin(), path, ConfigValueType.OBJECT.name(), other.name())
      }

  }

  implicit val inetSocketAddressReader: ValueReader[InetSocketAddress] = { (config: Config, path: String) =>
    val uri = new URI(s"my://${config.getString(path)}")
    new InetSocketAddress(uri.getHost, uri.getPort)
  }

  def loadConfig(userConfig: Config): Config = {
    loadConfig(Some(userConfig))
  }

  def loadConfig(maybeUserConfig: Option[Config]): Config = {
    val defaults = ConfigFactory.defaultOverrides()
    val external = maybeUserConfig
      .fold(defaults)(defaults.withFallback)

    val withApp = external
      .withFallback(ConfigFactory.defaultApplication())

    val directoryDefaults = ConfigFactory
      .parseString(s"waves.directory = ${defaultDirectory(withApp)}")

    external
      .withFallback(directoryDefaults)
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()
  }

  //noinspection SpellCheckingInspection
  def defaultDirectory(config: Config): String = {
    def osxDefaultDirectory: String =
      "${user.home}/Library/Application Support"

    def winDefaultDirectory: String =
      "${LOCALAPPDATA}"

    def nixDefaultDirectory: String = {
      val maybeXdgDir = sys.env.get("XDG_DATA_HOME")
      val defaultDir = "${user.home}/.local/share"

      maybeXdgDir getOrElse defaultDir
    }

    def withNetwork(config: Config): String = {
      val bc = config.getString("waves.blockchain.type")
      val suffix =
        if (bc == "CUSTOM") s"custom${config.getString("waves.blockchain.custom.address-scheme-character")}"
        else bc.toLowerCase

      s"waves-$suffix"
    }

    val parent =
      if (SystemUtils.IS_OS_WINDOWS) winDefaultDirectory
      else if (SystemUtils.IS_OS_MAC) osxDefaultDirectory
      else nixDefaultDirectory

    s"$parent/${withNetwork(config)}"
  }
}
