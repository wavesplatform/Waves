package com.wavesplatform.state

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.NameMapper

case class Settings(
    networkConfigFile: String,
    aliasesFile: String,
    restTxsFile: String,
    blocksFile: String,
    accountsFile: String,
    assetsFile: String,
    dataFile: String
)

object Settings {
  def fromConfig(config: Config): Settings = {
    implicit val nameMapper: NameMapper = net.ceedubs.ficus.readers.namemappers.HyphenNameMapper
    import net.ceedubs.ficus.readers.ArbitraryTypeReader._
    config.as[Settings]("waves.benchmark.state")
  }
}
