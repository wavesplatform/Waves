package com.wavesplatform.state2

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class Settings(dbPath: String,
                    aliasesFile: String,
                    aliasesFromHeight: Int,
                    restTxsFile: String,
                    restTxsFromHeight: Int,
                    accountsFile: String,
                    accountsFromHeight: Int,
                    assetsFile: String,
                    assetsFromHeight: Int,
                    dataFile: String,
                    dataFromHeight: Int)

object Settings {
  def fromConfig(config: Config): Settings = {
    implicit val _ = net.ceedubs.ficus.readers.namemappers.HyphenNameMapper
    config.as[Settings]("waves.benchmark.state2")
  }
}
