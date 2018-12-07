package com.wavesplatform.utx

import akka.event.EventStream
import cats.kernel.Monoid
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.model.Events.BalanceChanged
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.state.{ByteStr, Diff, Portfolio}
import com.wavesplatform.account.Address
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.transaction.{AssetId, Authorized, Transaction, ValidationError}

import scala.collection.mutable

class MatcherUtxPool(underlying: UtxPool, matcherSettings: MatcherSettings, events: EventStream) extends UtxPool with ScorexLogging {
  override def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)] = {
    underlying
      .putIfNew(tx)
      .map {
        case r @ (isNew, txDiff) =>
          if (isNew) {
            val msg = enrichWithAccountPortfolio {
              txDiff.portfolios.collect { case (addr, p) if addr.stringRepr != matcherSettings.account => (addr, p.pessimistic) }
            }
            if (!msg.isEmpty) {
              log.debug(s"Sending to event bus: $msg")
              events.publish(msg)
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

  override def packUnconfirmed(rest: MultiDimensionalMiningConstraint): (Seq[Transaction], MultiDimensionalMiningConstraint) =
    underlying.packUnconfirmed(rest)

  override def batched[Result](f: UtxBatchOps => Result): Result = {
    val ops = new BatchOpsImpl(underlying.createBatchOps)
    val r   = f(ops)
    val xs  = ops.messages
    if (xs.nonEmpty) {
      log.debug(s"Changed ${xs.map(_.changes.size).sum} accounts")
      xs.foreach(events.publish)
    }
    r
  }

  override private[utx] def createBatchOps: UtxBatchOps = new BatchOpsImpl(underlying.createBatchOps)

  private class BatchOpsImpl(underlying: UtxBatchOps) extends UtxBatchOps {
    private val accountInfos: mutable.Map[Address, Portfolio] = mutable.Map.empty

    def messages: Seq[BalanceChanged] = accountInfos.toMap.grouped(3).map(enrichWithAccountPortfolio).toSeq

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
            val prevPortfolio       = accountInfos.getOrElse(addr, Monoid[Portfolio].empty)
            val overriddenPortfolio = Monoid.combine(prevPortfolio, txPortfolio.pessimistic)
            accountInfos.update(addr, overriddenPortfolio)
          }
      }
    }
  }

  private def enrichWithAccountPortfolio(changes: Map[Address, Portfolio]): BalanceChanged = BalanceChanged {
    changes
      .flatMap {
        case (addr, portfolio) =>
          val changedAssets: Set[Option[AssetId]] = portfolio.assets.keySet.map(Some(_)) ++ {
            if (portfolio.effectiveBalance == 0) List.empty
            else List(None)
          }

          if (changedAssets.isEmpty) List.empty
          else {
            val accPortfolio = accountPortfolio(addr)
            val combined     = Monoid.combine(portfolio, accPortfolio)
            List(addr -> BalanceChanged.Changes(combined, changedAssets))
          }
      }
  }
}
