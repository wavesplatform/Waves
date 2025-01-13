package com.wavesplatform
import com.wavesplatform.block.Block
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult
import com.wavesplatform.state.{Blockchain, StateSnapshot}
import com.wavesplatform.transaction.Transaction

package object mining {
  private[mining] def createConstConstraint(maxSize: Long, transactionSize: => Long, description: String) = OneDimensionalMiningConstraint(
    maxSize,
    new com.wavesplatform.mining.TxEstimators.Fn {
      override def apply(b: Blockchain, t: Transaction, s: StateSnapshot): Long = transactionSize
      override val minEstimate                                                  = transactionSize
      override val toString: String                                             = s"const($transactionSize)"
    },
    description
  )

  type Appender = Block => Either[ValidationError, BlockApplyResult]
}
