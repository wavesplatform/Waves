package com.wavesplatform.it.tools

import com.wavesplatform.settings.WalletSettings
import com.wavesplatform.wallet.Wallet
import java.io.File
import java.nio.charset.StandardCharsets

import com.wavesplatform.account.AddressScheme

// dex-it/test:runMain com.wavesplatform.it.tools.WalletGeneratorApp
object WalletGeneratorApp extends App {
  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'Y'
  }

  val w = Wallet(
    WalletSettings(
      file = Some(new File("/Users/vsuharnikov/work/waves/node/dex/src/it/container/wallet.dat")),
      password = Some("some string as password"),
      seed = Some("seed".getBytes(StandardCharsets.UTF_8))
    ))

  w.generateNewAccount(0)
  w.generateNewAccount(808464433)
}
