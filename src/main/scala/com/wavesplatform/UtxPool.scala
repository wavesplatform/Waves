package com.wavesplatform

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats._
import com.google.common.cache.CacheBuilder
import com.wavesplatform.UtxPool.PessimisticPortfolios
import com.wavesplatform.settings.{FunctionalitySettings, UtxSettings}
import com.wavesplatform.state2.diffs.TransactionDiffer
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import com.wavesplatform.state2.{ByteStr, Diff, Instrumented, Portfolio}
import kamon.Kamon
import kamon.metric.instrument.{Time => KamonTime}
import scorex.account.Address
import scorex.consensus.TransactionsOrdering
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.utils.{ScorexLogging, Synchronized, Time}

import scala.concurrent.duration._
import scala.util.{Left, Right}

class UtxPool(time: Time,
              stateReader: StateReader,
              history: History,
              feeCalculator: FeeCalculator,
              fs: FunctionalitySettings,
              utxSettings: UtxSettings) extends Synchronized with ScorexLogging with Instrumented {

  def synchronizationToken: ReentrantReadWriteLock = new ReentrantReadWriteLock()

  private val transactions = Synchronized(Map.empty[ByteStr, Transaction])

  private lazy val knownTransactions = Synchronized {
    CacheBuilder
      .newBuilder()
      .maximumSize(utxSettings.maxSize * 2)
      .build[ByteStr, Either[ValidationError, Transaction]]()
  }

  private val pessimisticPortfolios = Synchronized(new PessimisticPortfolios)

  private val sizeStats = Kamon.metrics.histogram("utx-pool-size")
  private val processingTimeStats = Kamon.metrics.histogram(
    "utx-transaction-processing-time",
    KamonTime.Milliseconds
  )
  private val putRequestStats = Kamon.metrics.counter("utx-pool-put-if-new")

  private def removeExpired(currentTs: Long): Unit = write { implicit l =>
    def isExpired(tx: Transaction) = (currentTs - tx.timestamp).millis > utxSettings.maxTransactionAge

    transactions()
      .values
      .view
      .filter(isExpired)
      .foreach { tx =>
        transactions.transform(_ - tx.id)
        pessimisticPortfolios.mutate(_.remove(tx.id))
      }
  }

  def putIfNew(tx: Transaction): Either[ValidationError, Boolean] = write { implicit l =>
    putRequestStats.increment()
    measureSuccessful(processingTimeStats, {
      knownTransactions.mutate(cache =>
        Option(cache.getIfPresent(tx.id)) match {
          case Some(Right(_)) => Right(false)
          case Some(Left(er)) => Left(er)
          case None =>
            val res = for {
              _ <- Either.cond(transactions().size < utxSettings.maxSize, (), GenericError("Transaction pool size limit is reached"))
              _ <- feeCalculator.enoughFee(tx)
              diff <- TransactionDiffer(fs, history.lastBlockTimestamp(), time.correctedTime(), stateReader.height)(stateReader, tx)
            } yield {
              pessimisticPortfolios.mutate(_.add(tx.id, diff))
              transactions.transform(_.updated(tx.id, tx))
              tx
            }
            cache.put(tx.id, res)
            sizeStats.record(transactions().size)
            res.right.map(_ => true)
        })
    })
  }

  def removeAll(tx: Traversable[Transaction]): Unit = write { implicit l =>
    removeExpired(time.correctedTime())
    tx.view.map(_.id).foreach { id =>
      knownTransactions.mutate(_.invalidate(id))
      transactions.transform(_ - id)
      pessimisticPortfolios.mutate(_.remove(id))
    }
  }

  def portfolio(addr: Address): Portfolio = read { implicit l =>
    val base = stateReader.accountPortfolio(addr)
    val foundInUtx = pessimisticPortfolios().getAggregated(addr)

    Monoid.combine(base, foundInUtx)
  }

  def all(): Seq[Transaction] = read { implicit l =>
    transactions().values.toSeq.sorted(TransactionsOrdering.InUTXPool)
  }

  def size: Int = read { implicit l => transactions().size }

  def transactionById(transactionId: ByteStr): Option[Transaction] = read { implicit l =>
    transactions().get(transactionId)
  }

  def packUnconfirmed(max: Int, sortInBlock: Boolean): Seq[Transaction] = write { implicit l =>
    val currentTs = time.correctedTime()
    removeExpired(currentTs)
    val differ = TransactionDiffer(fs, history.lastBlockTimestamp(), currentTs, stateReader.height) _
    val (invalidTxs, reversedValidTxs, _) = transactions()
      .values.toSeq
      .sorted(TransactionsOrdering.InUTXPool)
      .foldLeft((Seq.empty[ByteStr], Seq.empty[Transaction], Monoid[Diff].empty)) {
        case ((invalid, valid, diff), tx) if valid.size <= max =>
          differ(new CompositeStateReader(stateReader, diff.asBlockDiff), tx) match {
            case Right(newDiff) if valid.size < max =>
              (invalid, tx +: valid, Monoid.combine(diff, newDiff))
            case Right(_) =>
              (invalid, valid, diff)
            case Left(_) =>
              (tx.id +: invalid, valid, diff)
          }
        case (r, _) => r
      }

    transactions.transform(_ -- invalidTxs)
    pessimisticPortfolios.mutate { p =>
      invalidTxs.foreach(p.remove)
    }
    if (sortInBlock)
      reversedValidTxs.sorted(TransactionsOrdering.InBlock)
    else reversedValidTxs.reverse
  }
}

object UtxPool {

  private class PessimisticPortfolios {
    private type Portfolios = Map[Address, Portfolio]

    private var transactionPortfolios = Map.empty[ByteStr, Portfolios]
    private var transactions = Map.empty[Address, Set[ByteStr]]

    def add(txId: ByteStr, txDiff: Diff): Unit = {
      val nonEmptyPessimisticPortfolios = txDiff.portfolios
        .mapValues(_.pessimistic)
        .filterNot {
          case (_, portfolio) => portfolio.isEmpty
        }

      if (nonEmptyPessimisticPortfolios.nonEmpty) {
        transactionPortfolios += txId -> nonEmptyPessimisticPortfolios
        nonEmptyPessimisticPortfolios.keys.foreach { address =>
          transactions += address -> (transactions.getOrElse(address, Set.empty) + txId)
        }
      }
    }

    def getAggregated(accountAddr: Address): Portfolio = {
      val portfolios = for {
        txIds <- transactions.get(accountAddr).toSeq
        txId <- txIds
        txPortfolios <- transactionPortfolios.get(txId)
        txAccountPortfolio <- txPortfolios.get(accountAddr)
      } yield txAccountPortfolio

      Monoid.combineAll[Portfolio](portfolios)
    }

    def remove(txId: ByteStr): Unit = {
      transactionPortfolios -= txId
      transactions = transactions.map { case (k, v) => k -> (v - txId) }
    }
  }

}
