package com.wavesplatform.utx

import akka.event.EventStream
import cats.kernel.Monoid
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.model.Events.BalanceChanged
import com.wavesplatform.mining.TwoDimensionalMiningConstraint
import com.wavesplatform.state2.{ByteStr, Diff, Portfolio}
import scorex.account.Address
import scorex.transaction.{Authorized, Transaction, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.mutable

class MatcherUtxPool(underlying: UtxPool, matcherSettings: MatcherSettings, events: EventStream) extends UtxPool with ScorexLogging {
  override def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)] = {
    underlying
      .putIfNew(tx)
      .map {
        case r @ (isNew, txDiff) =>
          if (isNew) {
            val xs = txDiff.portfolios.filterNot { case (addr, _) => addr.stringRepr == matcherSettings.account }
            if (xs.nonEmpty) {
              events.publish(BalanceChanged(enrichWithAccountPortfolio(xs)))
            }
          }
          r
      }
  }

  override def close(): Unit = underlying.close()

  override def removeAll(txs: Traversable[Transaction]): Unit = underlying.removeAll(txs)

  override def accountPortfolio(addr: Address): Portfolio = underlying.accountPortfolio(addr)

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
    private val accountInfos: mutable.Map[Address, Portfolio] = mutable.Map.empty

    def messages: Seq[BalanceChanged] = enrichWithAccountPortfolio(accountInfos.toMap).grouped(3).map(BalanceChanged(_)).toSeq

    override def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)] =
      underlying
        .putIfNew(tx)
        .map {
          case r @ (added, txDiff) =>
            if (added) {
              log.debug(s"Checking $tx, ${tx.isInstanceOf[Authorized]}")
              tx match {
                case _: Authorized => overridePortfolios(txDiff)
                case _             =>
              }
            }
            r
        }

    private def overridePortfolios(txDiff: Diff): Unit = {
      txDiff.portfolios.foreach {
        case (addr, txPortfolio) =>
          if (addr.stringRepr != matcherSettings.account) {
            val prevPortfolio = accountInfos.getOrElse(addr, Monoid[Portfolio].empty)
            val overriddenPortfolio = prevPortfolio.assets.foldLeft(txPortfolio) {
              case (r, (assetId, balance)) =>
                val fallbackBalance = r.assets.getOrElse(assetId, balance)
                r.copy(assets = r.assets.updated(assetId, fallbackBalance))
            }
            accountInfos.update(addr, overriddenPortfolio)
          }
      }
    }
  }

  private def enrichWithAccountPortfolio(changes: Map[Address, Portfolio]): Map[Address, Portfolio] = {
    changes.map { case (addr, portfolio) =>
      val accPortfolio = accountPortfolio(addr)
      val filteredPortfolio = accPortfolio.copy(assets = accPortfolio.assets.filter {
        case (assetId, _) => portfolio.assets.contains(assetId)
      })
      addr -> Monoid.combine(portfolio, filteredPortfolio)
    }
  }
}
