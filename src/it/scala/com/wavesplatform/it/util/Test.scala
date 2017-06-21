package com.wavesplatform.it.util

import scorex.account.PrivateKeyAccount

object Test extends App {
  val accounts = Seq(PrivateKeyAccount(Array.emptyByteArray), PrivateKeyAccount(Array[Byte](1)), PrivateKeyAccount(Array[Byte](1,1)))
  val txs = TransactionGenerator.gen(accounts, 5000)
  println(txs)
}
