package com.wavesplatform.settings

import com.typesafe.config.Config

import net.ceedubs.ficus.Ficus._

case class WalletSettings(file: String, password: String, seed: String)

object WalletSettings {
  val configPath = "waves.wallet"

  def fromConfig(config: Config): WalletSettings = {
    val file = config.as[String](s"$configPath.file")
    val password = config.as[String](s"$configPath.password")
    val seed = config.as[String](s"$configPath.seed")

    WalletSettings(file, password, seed)
  }
}