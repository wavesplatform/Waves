package com.wavesplatform.generator

import com.wavesplatform.transaction.Transaction

trait TransactionGenerator extends Iterator[Iterator[Transaction]] {
  override val hasNext          = true
  def initial: Seq[Transaction] = Seq.empty
}
