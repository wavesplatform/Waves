package com.wavesplatform

import com.wavesplatform.settings.WalletSettings
import scorex.wallet.Wallet

trait TestWallet {
  protected val testWallet = {
    val wallet = Wallet(WalletSettings(None, "123", ""))
    wallet.generateNewAccounts(10)
    wallet
  }
}
