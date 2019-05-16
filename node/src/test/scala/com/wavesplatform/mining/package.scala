package com.wavesplatform

import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.Transaction

package object mining {
  private[mining] def createConstConstraint(maxSize: Long, transactionSize: => Long) = OneDimensionalMiningConstraint(
    maxSize,
    new com.wavesplatform.mining.TxEstimators.Fn {
      override def apply(b: Blockchain, t: Transaction, d: Diff) = transactionSize
      override val minEstimate                          = transactionSize
    }
  )
}
