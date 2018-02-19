package com.wavesplatform.utx

import akka.actor.ActorRef
import com.wavesplatform.mining.TwoDimensionalMiningConstraint
import com.wavesplatform.state2.{ByteStr, Portfolio}
import scorex.account.Address
import scorex.transaction.{Transaction, ValidationError}

class MatcherUtxPool(underlying: UtxPool, matcher: ActorRef) extends UtxPool {
  override def putIfNew(tx: Transaction): Either[ValidationError, Boolean] = {
    underlying
      .putIfNew(tx)
      .map { r =>
        if (r) matcher ! tx
        r
      }
  }

  override def close(): Unit = underlying.close()
  override def removeAll(txs: Traversable[Transaction]): Unit = underlying.removeAll(txs)
  override def portfolio(addr: Address): Portfolio = underlying.portfolio(addr)
  override def all: Seq[Transaction] = underlying.all
  override def size: Int = underlying.size
  override def transactionById(transactionId: ByteStr): Option[Transaction] = underlying.transactionById(transactionId)

  override def packUnconfirmed(rest: TwoDimensionalMiningConstraint,
                               sortInBlock: Boolean)
    : (Seq[Transaction], TwoDimensionalMiningConstraint) = underlying.packUnconfirmed(rest, sortInBlock)

  override def batched(f: UtxBatchOps => Unit): Unit = super.batched(f)
  override def createBatchOps: UtxBatchOps = new BatchOpsImpl(underlying.createBatchOps)

  private class BatchOpsImpl(underlying: UtxBatchOps) extends UtxBatchOps {
    override def putIfNew(tx: Transaction): Either[ValidationError, Boolean] = underlying
      .putIfNew(tx)
      .map { r =>
        if (r) matcher ! tx
        r
      }
  }
}
