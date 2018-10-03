package com.wavesplatform.settings

import java.io.File

import com.wavesplatform.state.ByteStr

case class WalletSettings(file: Option[File], password: Option[String], seed: Option[ByteStr])
