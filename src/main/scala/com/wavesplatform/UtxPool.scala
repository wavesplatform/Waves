package com.wavesplatform

import java.util.concurrent.ConcurrentHashMap

import cats._
import com.wavesplatform.UtxPoolImpl.PessimisticPortfolios
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.settings.{FunctionalitySettings, UtxSettings}
import com.wavesplatform.state2.diffs.TransactionDiffer
import com.wavesplatform.state2.reader.CompositeStateReader.composite
import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{ByteStr, Diff, Portfolio, StateReader}
import kamon.Kamon
import kamon.metric.instrument.{Time => KamonTime}
import monix.eval.Task
import monix.execution.Scheduler
import scorex.account.Address
import scorex.consensus.TransactionsOrdering
import scorex.transaction.ValidationError.{GenericError, SenderIsBlacklisted}
import scorex.transaction._
import scorex.transaction.assets.TransferTransaction
import scorex.utils.{ScorexLogging, Time}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.{Left, Right}


trait UtxPool {

  def putIfNew(tx: Transaction): Either[ValidationError, Boolean]

  def removeAll(txs: Traversable[Transaction]): Unit

  def portfolio(addr: Address): Portfolio

  def all: Seq[Transaction]

  def size: Int

  def transactionById(transactionId: ByteStr): Option[Transaction]

  def packUnconfirmed(max: Int, sortInBlock: Boolean): Seq[Transaction]

  def batched(f: UtxBatchOps => Unit): Unit

}

trait UtxBatchOps {
  def putIfNew(tx: Transaction): Either[ValidationError, Boolean]
}

class UtxPoolImpl(time: Time,
                  stateReader: StateReader,
                  history: History,
                  feeCalculator: FeeCalculator,
                  fs: FunctionalitySettings,
                  utxSettings: UtxSettings) extends ScorexLogging with Instrumented with AutoCloseable with UtxPool {
  outer =>

  private implicit val scheduler: Scheduler = Scheduler.singleThread("utx-pool-cleanup")

  private val transactions = new ConcurrentHashMap[ByteStr, Transaction]()
  private val pessimisticPortfolios = new PessimisticPortfolios

  private val removeInvalid = Task {
    val state = stateReader()
    val transactionsToRemove = transactions.values.asScala.filter(t => state.containsTransaction(t.id()))
    removeAll(transactionsToRemove)
  }.delayExecution(utxSettings.cleanupInterval)

  private val cleanup = removeInvalid.flatMap(_ => removeInvalid).runAsyncLogErr

  override def close(): Unit = cleanup.cancel()

  private val utxPoolSizeStats = Kamon.metrics.minMaxCounter("utx-pool-size", 500.millis)
  private val processingTimeStats = Kamon.metrics.histogram("utx-transaction-processing-time", KamonTime.Milliseconds)
  private val putRequestStats = Kamon.metrics.counter("utx-pool-put-if-new")

  private def removeExpired(currentTs: Long): Unit = {
    def isExpired(tx: Transaction) = (currentTs - tx.timestamp).millis > utxSettings.maxTransactionAge

    transactions
      .values
      .asScala
      .filter(isExpired)
      .foreach { tx =>
        transactions.remove(tx.id())
        pessimisticPortfolios.remove(tx.id())
        utxPoolSizeStats.decrement()
      }
  }

  override def putIfNew(tx: Transaction): Either[ValidationError, Boolean] = putIfNew(stateReader(), tx)

  private def checkNotBlacklisted(tx: Transaction): Either[ValidationError, Unit] = {
    if (utxSettings.blacklistSenderAddresses.isEmpty) {
      Right(())
    } else {
      val sender: Option[String] = tx match {
        case x: SignedTransaction => Some(x.sender.address)
        case x: PaymentTransaction => Some(x.sender.address)
        case _ => None
      }

      val recipient: Option[String] = tx match {
        case x: TransferTransaction => Some(x.recipient.stringRepr)
        case _ => None
      }

      sender match {
        case None => Right(())
        case Some(addr) =>
          val blacklist = utxSettings.blacklistSenderAddresses.contains(addr)
          lazy val allowBlacklisted = recipient.exists(utxSettings.allowBlacklistedTransferTo.contains)
          if (blacklist && !allowBlacklisted) Left(SenderIsBlacklisted(addr)) else Right(())
      }
    }
  }

  override def removeAll(txs: Traversable[Transaction]): Unit = {
    txs.view.map(_.id()).foreach { id =>
      Option(transactions.remove(id)).foreach(_ => utxPoolSizeStats.decrement())
      pessimisticPortfolios.remove(id)
    }

    removeExpired(time.correctedTime())
  }

  override def portfolio(addr: Address): Portfolio = {
    val base = stateReader().accountPortfolio(addr)
    val foundInUtx = pessimisticPortfolios.getAggregated(addr)

    Monoid.combine(base, foundInUtx)
  }

  override def all: Seq[Transaction] = {
    transactions.values.asScala.toSeq.sorted(TransactionsOrdering.InUTXPool)
  }

  override def size: Int = transactions.size

  override def transactionById(transactionId: ByteStr): Option[Transaction] = Option(transactions.get(transactionId))

  override def packUnconfirmed(max: Int, sortInBlock: Boolean): Seq[Transaction] = {
    val currentTs = time.correctedTime()
    removeExpired(currentTs)
    val s = stateReader()
    val differ = TransactionDiffer(fs, history.lastBlockTimestamp(), currentTs, s.height) _
    val (invalidTxs, reversedValidTxs, _) = transactions
      .values.asScala.toSeq
      .sorted(TransactionsOrdering.InUTXPool)
      .foldLeft((Seq.empty[ByteStr], Seq.empty[Transaction], Monoid[Diff].empty)) {
        case ((invalid, valid, diff), tx) if valid.lengthCompare(max) <= 0 =>
          differ(composite(diff.asBlockDiff, s), tx) match {
            case Right(newDiff) if valid.lengthCompare(max) < 0 =>
              (invalid, tx +: valid, Monoid.combine(diff, newDiff))
            case Right(_) =>
              (invalid, valid, diff)
            case Left(_) =>
              (tx.id() +: invalid, valid, diff)
          }
        case (r, _) => r
      }

    invalidTxs.foreach { itx =>
      transactions.remove(itx)
      pessimisticPortfolios.remove(itx)
    }
    if (sortInBlock)
      reversedValidTxs.sorted(TransactionsOrdering.InBlock)
    else reversedValidTxs.reverse
  }

  override def batched(f: UtxBatchOps => Unit): Unit = f(new BatchOpsImpl(stateReader()))

  private class BatchOpsImpl(s: SnapshotStateReader) extends UtxBatchOps {
    override def putIfNew(tx: Transaction): Either[ValidationError, Boolean] = outer.putIfNew(s, tx)
  }

  private def putIfNew(s: SnapshotStateReader, tx: Transaction): Either[ValidationError, Boolean] = {
    putRequestStats.increment()
    measureSuccessful(processingTimeStats, {
      for {
        _ <- Either.cond(transactions.size < utxSettings.maxSize, (), GenericError("Transaction pool size limit is reached"))
        _ <- checkNotBlacklisted(tx)
        _ <- feeCalculator.enoughFee(tx)
        diff <- TransactionDiffer(fs, history.lastBlockTimestamp(), time.correctedTime(), s.height)(s, tx)
      } yield {
        utxPoolSizeStats.increment()
        pessimisticPortfolios.add(tx.id(), diff)
        Option(transactions.put(tx.id(), tx)).isEmpty
      }
    })
  }

}

object UtxPoolImpl {

  private class PessimisticPortfolios {
    private type Portfolios = Map[Address, Portfolio]
    private val transactionPortfolios = new ConcurrentHashMap[ByteStr, Portfolios]()
    private val transactions = new ConcurrentHashMap[Address, Set[ByteStr]]()

    def add(txId: ByteStr, txDiff: Diff): Unit = {
      val nonEmptyPessimisticPortfolios = txDiff.portfolios
        .mapValues(_.pessimistic)
        .filterNot {
          case (_, portfolio) => portfolio.isEmpty
        }

      if (nonEmptyPessimisticPortfolios.nonEmpty &&
        Option(transactionPortfolios.put(txId, nonEmptyPessimisticPortfolios)).isEmpty) {
        nonEmptyPessimisticPortfolios.keys.foreach { address =>
          transactions.put(address, transactions.getOrDefault(address, Set.empty) + txId)
        }
      }
    }

    def getAggregated(accountAddr: Address): Portfolio = {
      val portfolios = for {
        txId <- transactions.getOrDefault(accountAddr, Set.empty).toSeq
        txPortfolios = transactionPortfolios.getOrDefault(txId, Map.empty[Address, Portfolio])
        txAccountPortfolio <- txPortfolios.get(accountAddr).toSeq
      } yield txAccountPortfolio

      Monoid.combineAll[Portfolio](portfolios)
    }

    def remove(txId: ByteStr): Unit = {
      if (Option(transactionPortfolios.remove(txId)).isDefined) {
        transactions.keySet().asScala.foreach { addr =>
          transactions.put(addr, transactions.getOrDefault(addr, Set.empty) - txId)
        }
      }
    }
  }

}
