package com.wavesplatform.discovery

import java.net.InetSocketAddress

import net.ceedubs.ficus.Ficus._
import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.readers.{NameMapper, ValueReader}

import scala.concurrent.duration.FiniteDuration

case class ChainSettings(chainId: Char, initialPeers: Seq[InetSocketAddress])

case class Settings(chains: Seq[ChainSettings], webSocketHost: String, webSocketPort: Int, workersCount: Int, discoveryInterval: FiniteDuration)

object Settings {
  implicit val readConfigInHyphen: NameMapper = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase // IDEA bug

  implicit val inetSocketAddressReader: ValueReader[InetSocketAddress] = { (config: Config, path: String) =>
    val value = config.as[String](s"$path").split(":")
    new InetSocketAddress(
      value(0),
      value(1).toInt
    )
  }

  implicit val charReader: ValueReader[Char] = (config: Config, path: String) => config.as[String](s"$path").head

  import net.ceedubs.ficus.readers.ArbitraryTypeReader._

  lazy val default: Settings = ConfigFactory.load().as[Settings]("discovery")
}
