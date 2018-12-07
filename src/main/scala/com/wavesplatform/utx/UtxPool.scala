package com.wavesplatform.utx

import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.state.{ByteStr, Diff, Portfolio}
import com.wavesplatform.account.Address
import com.wavesplatform.transaction._

trait UtxPool extends AutoCloseable {
  self =>

  def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)]

  def removeAll(txs: Traversable[Transaction]): Unit

  def accountPortfolio(addr: Address): Portfolio

  def portfolio(addr: Address): Portfolio

  def all: Seq[Transaction]

  def size: Int

  def transactionById(transactionId: ByteStr): Option[Transaction]

  def packUnconfirmed(rest: MultiDimensionalMiningConstraint): (Seq[Transaction], MultiDimensionalMiningConstraint)

  def batched[Result](f: UtxBatchOps => Result): Result = f(createBatchOps)

  private[utx] def createBatchOps: UtxBatchOps = new UtxBatchOps {
    override def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)] = self.putIfNew(tx)
  }

}

trait UtxBatchOps {
  def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)]
}
