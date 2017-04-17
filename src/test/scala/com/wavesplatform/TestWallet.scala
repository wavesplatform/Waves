package com.wavesplatform

import scorex.wallet.Wallet

trait TestWallet {
  protected val testWallet = {
    val file = scorex.createTestTemporaryFile("wallet", ".dat")
    val wallet = new Wallet(Some(file.getCanonicalPath), "123", None)
    wallet.generateNewAccounts(10)
    wallet
  }
}
