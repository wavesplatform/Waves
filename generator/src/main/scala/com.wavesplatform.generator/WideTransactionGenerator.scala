package com.wavesplatform.generator

import scorex.account.PrivateKeyAccount
import scorex.transaction.Transaction

class WideTransactionGenerator(val accounts: Seq[PrivateKeyAccount]) extends TransactionGenerator {
  override def generate(count: Int): Seq[Transaction] = {
    Seq.empty[Transaction]
  }

}
