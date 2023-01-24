package com.wavesplatform

import com.wavesplatform.wallet.Wallet

import java.nio.charset.StandardCharsets

trait HasTestAccounts {
  protected val scriptedAcc          = Wallet.generateNewAccount("scripted".getBytes(StandardCharsets.UTF_8), 0)
  protected lazy val scriptedAccAddr = scriptedAcc.toAddress

  protected val alice          = Wallet.generateNewAccount("alice".getBytes(StandardCharsets.UTF_8), 1)
  protected lazy val aliceAddr = alice.toAddress

  protected val bob          = Wallet.generateNewAccount("bob".getBytes(StandardCharsets.UTF_8), 2)
  protected lazy val bobAddr = bob.toAddress
}
