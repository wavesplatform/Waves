package com.wavesplatform

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings.WalletSettings
import com.wavesplatform.wallet.Wallet

trait TestWallet {
  protected lazy val testWallet: Wallet = Wallet(WalletSettings(None, Some("123"), Some(ByteStr.empty)))
}
