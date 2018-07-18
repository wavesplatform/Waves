package com.wavesplatform

import com.wavesplatform.settings.WalletSettings
import com.wavesplatform.wallet.Wallet

trait TestWallet {
  protected val testWallet = {
    val wallet = Wallet(WalletSettings(None, "123", None))
    wallet.generateNewAccounts(10)
    wallet
  }
}
