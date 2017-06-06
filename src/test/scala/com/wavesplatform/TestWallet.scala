package com.wavesplatform

import scorex.wallet.Wallet

trait TestWallet {
  protected val testWallet = {
    val wallet = new Wallet(None, "123".toCharArray, None)
    wallet.generateNewAccounts(10)
    wallet
  }
}
