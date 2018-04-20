package com.wavesplatform.state2

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class Settings(dbPath: String, aliasesFile: String, restTxsFile: String, accountsFile: String, assetsFile: String, dataFile: String)

object Settings {
  def fromConfig(config: Config): Settings = {
    implicit val _ = net.ceedubs.ficus.readers.namemappers.HyphenNameMapper
    config.as[Settings]("waves.benchmark.state2")
  }
}
