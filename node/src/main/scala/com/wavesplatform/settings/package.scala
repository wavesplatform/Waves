package com.wavesplatform

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.common.state.ByteStr
import pureconfig.*
import pureconfig.ConvertHelpers.catchReadError
import pureconfig.configurable.genericMapReader
import pureconfig.error.{CannotConvert, ConfigReaderFailures}
import supertagged.TaggedType

import scala.util.Try

package object settings {
  extension (objCur: ConfigObjectCursor) {
    def required[T](key: String)(using reader: ConfigReader[T]): Either[ConfigReaderFailures, T] =
      objCur.atKey(key).flatMap(ConfigReader[T].from)

    def optionalWithDefault[T](key: String, default: T)(using reader: ConfigReader[T]): Either[ConfigReaderFailures, T] =
      ConfigReader[Option[T]].from(objCur.atKeyOrUndefined(key)).map(_.getOrElse(default))
  }

  implicit val byteStrReader: ConfigReader[ByteStr] =
    ConfigReader.fromString(str => ByteStr.decodeBase58(str).toEither.left.map(e => CannotConvert(str, "ByteStr", e.getMessage)))
  implicit val preactivatedFeaturesReader: ConfigReader[Map[Short, Int]] = genericMapReader(catchReadError(_.toShort))

  implicit val privateKeyReader: ConfigReader[PrivateKey] = ConfigReader[ByteStr].map(PrivateKey(_))

  object SizeInBytes extends TaggedType[Long]
  type SizeInBytes = SizeInBytes.Type

  implicit val sizeInBytesConfigReader: ConfigReader[SizeInBytes] = ConfigReader.fromCursor(cur =>
    for {
      configValue <- cur.asConfigValue
    } yield {
      val config      = ConfigFactory.empty().withValue("stubKey", configValue)
      val bytes: Long = config.getBytes("stubKey")
      SizeInBytes(bytes)
    }
  )

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
        } else
          bc.toLowerCase

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
