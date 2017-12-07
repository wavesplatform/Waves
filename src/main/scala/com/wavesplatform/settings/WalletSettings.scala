package com.wavesplatform.settings

import java.io.File

import com.wavesplatform.state2.ByteStr

case class WalletSettings(file: Option[File], password: String, seed: Option[ByteStr])
