package com.wavesplatform.settings

import com.wavesplatform.state2.ByteStr

case class WalletSettings(password: String, seed: Option[ByteStr])
