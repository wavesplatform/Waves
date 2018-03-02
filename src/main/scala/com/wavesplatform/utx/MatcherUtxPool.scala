package com.wavesplatform.utx

import akka.event.EventStream
import com.wavesplatform.matcher.model.Events.BalanceChanged
import com.wavesplatform.mining.TwoDimensionalMiningConstraint
import com.wavesplatform.state2.{ByteStr, Portfolio}
import scorex.account.Address
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.{Authorized, Transaction, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.mutable

class MatcherUtxPool(underlying: UtxPool, events: EventStream) extends UtxPool with ScorexLogging {
  override def putIfNew(tx: Transaction): Either[ValidationError, Boolean] = {
    underlying
      .putIfNew(tx)
      .map { r =>
        if (r) {
          tx match {
            case _: ExchangeTransaction => // Ignoring, because the sender is the matcher
            case tx: Authorized         => events.publish(BalanceChanged(Map(tx.sender.address -> portfolio(tx.sender))))
            case _                      =>
          }
        }
        r
      }
  }

  override def close(): Unit = underlying.close()

  override def removeAll(txs: Traversable[Transaction]): Unit = underlying.removeAll(txs)

  override def portfolio(addr: Address): Portfolio = underlying.portfolio(addr)

  override def all: Seq[Transaction] = underlying.all

  override def size: Int = underlying.size

  override def transactionById(transactionId: ByteStr): Option[Transaction] = underlying.transactionById(transactionId)

  override def packUnconfirmed(rest: TwoDimensionalMiningConstraint, sortInBlock: Boolean): (Seq[Transaction], TwoDimensionalMiningConstraint) =
    underlying.packUnconfirmed(rest, sortInBlock)

  override def batched[Result](f: UtxBatchOps => Result): Result = {
    val ops = new BatchOpsImpl(underlying.createBatchOps)
    val r   = f(ops)
    val xs  = ops.messages
    if (xs.nonEmpty) {
      log.debug(s"Changed ${xs.map(_.changesByAddress.size).sum} accounts")
      xs.foreach(events.publish)
    }
    r
  }

  override private[utx] def createBatchOps: UtxBatchOps = new BatchOpsImpl(underlying.createBatchOps)

  private class BatchOpsImpl(underlying: UtxBatchOps) extends UtxBatchOps {
    private val accountInfos: mutable.Map[String, Portfolio] = mutable.Map.empty

    def messages: Seq[BalanceChanged] = accountInfos.toMap.grouped(3).map(BalanceChanged(_)).toSeq

    override def putIfNew(tx: Transaction): Either[ValidationError, Boolean] =
      underlying
        .putIfNew(tx)
        .map { r =>
          if (r) {
            log.debug(s"Checking $tx, ${tx.isInstanceOf[Authorized]}")
            tx match {
              case tx: Authorized => accountInfos += tx.sender.address -> portfolio(tx.sender)
              case _              =>
            }
          }
          r
        }
  }
}
