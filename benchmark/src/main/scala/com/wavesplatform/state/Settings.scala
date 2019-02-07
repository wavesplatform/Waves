package com.wavesplatform.state

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class Settings(networkConfigFile: String,
                    aliasesFile: String,
                    aliasesFromHeight: Int,
                    restTxsFile: String,
                    restTxsFromHeight: Int,
                    txsAddressesFile: String,
                    txsAddressesFromHeight: Int,
                    blocksFile: String,
                    blocksFromHeight: Int,
                    accountsFile: String,
                    accountsFromHeight: Int,
                    assetsFile: String,
                    assetsFromHeight: Int,
                    dataFile: String,
                    dataFromHeight: Int)

object Settings {
  def fromConfig(config: Config): Settings = {
    implicit val _ = net.ceedubs.ficus.readers.namemappers.HyphenNameMapper
    config.as[Settings]("waves.benchmark.state")
  }
}
