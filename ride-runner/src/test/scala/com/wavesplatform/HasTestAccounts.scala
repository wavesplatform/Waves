package com.wavesplatform

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.wallet.Wallet

import java.nio.charset.StandardCharsets

trait HasTestAccounts { this: BaseTestSuite =>
  protected val miner = Wallet.generateNewAccount("miner".getBytes(StandardCharsets.UTF_8), 0)

  protected lazy val (scriptedAcc, scriptedAccAddr) = mkKpAndAddr("scripted")
  protected lazy val (alice, aliceAddr)             = mkKpAndAddr("alice")
  protected lazy val (bob, bobAddr)                 = mkKpAndAddr("bob")
  protected lazy val (carl, carlAddr)               = mkKpAndAddr("carl")

  protected def mkKpAndAddr(seed: String): (KeyPair, Address) = {
    val wallet = Wallet.generateNewAccount(seed.getBytes(StandardCharsets.UTF_8), 0)
    (wallet, wallet.toAddress(chainId))
  }
}
