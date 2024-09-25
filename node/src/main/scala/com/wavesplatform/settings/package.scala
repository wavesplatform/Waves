package com.wavesplatform

import java.io.File
import java.net.{InetSocketAddress, URI}
import cats.data.NonEmptyList
import com.typesafe.config.{Config, ConfigException, ConfigFactory, ConfigValueType}
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.common.state.ByteStr
import net.ceedubs.ficus.Ficus.traversableReader
import net.ceedubs.ficus.readers.namemappers.HyphenNameMapper
import net.ceedubs.ficus.readers.{NameMapper, ValueReader}
import supertagged.TaggedType

import scala.jdk.CollectionConverters.*
import scala.util.Try

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

  implicit val byteReader: ValueReader[Byte] = { (cfg: Config, path: String) =>
    val x = cfg.getInt(path)
    if (x.isValidByte) x.toByte
    else throw new ConfigException.WrongType(cfg.origin(), s"$path has an invalid value: '$x' expected to be a byte")
  }

  implicit val inetSocketAddressReader: ValueReader[InetSocketAddress] = { (config: Config, path: String) =>
    val uri = new URI(s"my://${config.getString(path)}")
    new InetSocketAddress(uri.getHost, uri.getPort)
  }

  implicit val privateKeyReader: ValueReader[PrivateKey] = byteStrReader.map(PrivateKey(_))

  implicit def nonEmptyListReader[T: ValueReader]: ValueReader[NonEmptyList[T]] = implicitly[ValueReader[List[T]]].map {
    case Nil     => throw new IllegalArgumentException("Expected at least one element")
    case x :: xs => NonEmptyList(x, xs)
  }

  object SizeInBytes extends TaggedType[Long]
  type SizeInBytes = SizeInBytes.Type

  implicit val sizeInBytesReader: ValueReader[SizeInBytes] = { (cfg: Config, path: String) =>
    SizeInBytes(cfg.getBytes(path).toLong)
  }

  def loadConfig(userConfig: Config): Config = {
    loadConfig(Some(userConfig))
  }

  def loadConfig(maybeUserConfig: Option[Config]): Config = {
    val sysProps = ConfigFactory.defaultOverrides()
    val external = maybeUserConfig.fold(sysProps)(sysProps.withFallback)

    val cmdDefaults =
      Try(external.getConfig("waves.defaults"))
        .getOrElse(ConfigFactory.empty())
        .atPath("waves")

    val withApp = external.withFallback(cmdDefaults).withFallback(ConfigFactory.defaultApplication())

    val networkDefaults = {
      val network = withApp.getString("waves.blockchain.type").toLowerCase
      withApp.getConfig(s"waves.defaults.$network")
    }

    external
      .withFallback(cmdDefaults)
      .withFallback(networkDefaults.atKey("waves"))
      .withFallback(ConfigFactory.parseString(s"waves.directory = ${defaultDirectory(withApp)}"))
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()
  }

  def defaultDirectory(config: Config): String = {
    // No actual interpolation here, `s` to suppress warnings
    def osxDefaultDirectory: String =
      s"$${user.home}/Library/Application Support"

    // noinspection SpellCheckingInspection
    def winDefaultDirectory: String =
      s"$${LOCALAPPDATA}"

    def nixDefaultDirectory: String = {
      val maybeXdgDir = sys.env.get("XDG_DATA_HOME")
      val defaultDir  = s"$${user.home}/.local/share"

      maybeXdgDir getOrElse defaultDir
    }

    def withNetwork(config: Config): String = {
      val bc = config.getString("waves.blockchain.type")
      val suffix =
        if (bc == "CUSTOM") {
          val char = config.getString("waves.blockchain.custom.address-scheme-character").headOption.getOrElse(0.toChar)
          s"custom-${Integer.toHexString(char)}"
        } else bc.toLowerCase

      s"waves-$suffix"
    }

    val osName = sys.props.get("os.name").map(_.toLowerCase)
    val parent =
      if (osName.exists(_.contains("win"))) winDefaultDirectory
      else if (osName.exists(_.contains("mac"))) osxDefaultDirectory
      else nixDefaultDirectory

    s"$parent/${withNetwork(config)}"
  }
}
