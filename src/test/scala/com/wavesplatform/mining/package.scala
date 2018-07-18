package com.wavesplatform

import com.wavesplatform.state.Blockchain
import scorex.transaction.Transaction

package object mining {
  private[mining] def createConstConstraint(maxSize: Long, transactionSize: => Long = ???) = OneDimensionalMiningConstraint(
    maxSize,
    new com.wavesplatform.mining.TxEstimators.Fn {
      def apply(b: Blockchain, t: Transaction) = transactionSize
      val minEstimate                          = transactionSize
    }
  )
}
