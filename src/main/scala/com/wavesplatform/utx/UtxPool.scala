package com.wavesplatform.utx

import com.wavesplatform.mining.TwoDimensionalMiningConstraint
import com.wavesplatform.state2.{ByteStr, Portfolio}
import scorex.account.Address
import scorex.transaction._

trait UtxPool extends AutoCloseable {

  def putIfNew(tx: Transaction): Either[ValidationError, Boolean]

  def removeAll(txs: Traversable[Transaction]): Unit

  def portfolio(addr: Address): Portfolio

  def all: Seq[Transaction]

  def size: Int

  def transactionById(transactionId: ByteStr): Option[Transaction]

  def packUnconfirmed(rest: TwoDimensionalMiningConstraint, sortInBlock: Boolean): (Seq[Transaction], TwoDimensionalMiningConstraint)

  def batched[Result](f: UtxBatchOps => Result): Result = f(createBatchOps)

  protected def createBatchOps: UtxBatchOps

}

trait UtxBatchOps {
  def putIfNew(tx: Transaction): Either[ValidationError, Boolean]
}
