package com.wavesplatform.settings

import pureconfig.*
import java.io.File

import com.wavesplatform.common.state.ByteStr

case class WalletSettings(file: Option[File], password: Option[String], seed: Option[ByteStr]) derives ConfigReader
