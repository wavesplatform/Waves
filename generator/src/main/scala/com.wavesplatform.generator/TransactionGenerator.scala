package com.wavesplatform.generator

import scorex.account.PrivateKeyAccount
import scorex.transaction.Transaction

trait TransactionGenerator {

  val accounts: Seq[PrivateKeyAccount]

  def generate(count: Int): Seq[Transaction]

}
