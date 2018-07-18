package com.wavesplatform

import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Transaction

package object mining {
  private[mining] def createConstConstraint(maxSize: Long, transactionSize: => Long = ???) = OneDimensionalMiningConstraint(
    maxSize,
    (_: Blockchain, _: Transaction) => transactionSize
  )
}
