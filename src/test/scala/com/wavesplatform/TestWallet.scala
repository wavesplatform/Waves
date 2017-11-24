package com.wavesplatform

import com.wavesplatform.settings.WalletSettings
import scorex.wallet.Wallet

trait TestWallet extends TestDB {
  protected val testWallet = {
    val db = open()
    val wallet = Wallet(db, WalletSettings("123", None))
    wallet.generateNewAccounts(10)
    wallet
  }
}
