package com.wavesplatform.settings

import java.io.File

import com.typesafe.config.Config
import com.wavesplatform.state2.ByteStr
import net.ceedubs.ficus.Ficus._

case class WalletSettings(file: Option[File], password: String, seed: Option[ByteStr])

object WalletSettings {
  val configPath = "waves.wallet"

  def fromConfig(config: Config): WalletSettings = {
    val file = config.as[Option[File]](s"$configPath.file")
    val password = config.as[String](s"$configPath.password")
    val seed = config.as[Option[ByteStr]](s"$configPath.seed")

    WalletSettings(file, password, seed)
  }
}