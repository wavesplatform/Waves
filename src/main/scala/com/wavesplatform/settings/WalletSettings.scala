package com.wavesplatform.settings

import java.io.File
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import com.typesafe.config.Config
import com.wavesplatform.state2.ByteStr

case class WalletSettings(file: Option[File], password: String, seed: Option[ByteStr])

object WalletSettings {
  def fromConfig(config: Config): WalletSettings = {
    config.as[WalletSettings]("waves.wallet") match {
      case WalletSettings(None, p, s) => {
        val directory = config.as[String]("waves.directory")
        val defaultFile = new File(s"$directory/wallet/wallet.dat")

        WalletSettings(Some(defaultFile), p, s)
      }
      case ws => ws
    }
  }
}