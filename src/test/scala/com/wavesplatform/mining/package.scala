package com.wavesplatform

import com.wavesplatform.state.Blockchain
import scorex.block.Block
import scorex.transaction.Transaction

package object mining {
  private[mining] def createConstConstraint(maxSize: Long, blockSize: => Long = ???, transactionSize: => Long = ???) = OneDimensionalMiningConstraint(
    maxSize,
    new Estimator {
      override implicit def estimate(blockchain: Blockchain, x: Block): Long       = blockSize
      override implicit def estimate(blockchain: Blockchain, x: Transaction): Long = transactionSize
    }
  )
}
