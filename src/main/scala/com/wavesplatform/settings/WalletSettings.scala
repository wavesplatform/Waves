package com.wavesplatform.settings

import java.io.File

import com.wavesplatform.common.state.ByteStr
import net.ceedubs.ficus.readers.{ArbitraryTypeReader, ValueReader}
import net.ceedubs.ficus.Ficus._

case class WalletSettings(file: Option[File], password: Option[String], seed: Option[ByteStr])

object WalletSettings {
  implicit val walletSettingsValueReader: ValueReader[WalletSettings] = ArbitraryTypeReader.arbitraryTypeValueReader
}
