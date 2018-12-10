package com.wavesplatform.utx

import java.time.Duration
import java.time.temporal.ChronoUnit
import java.util.concurrent.ConcurrentHashMap

import cats._
import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.consensus.TransactionsOrdering
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.settings.{FunctionalitySettings, UtxSettings}
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.reader.CompositeBlockchain.composite
import com.wavesplatform.state.{Blockchain, ByteStr, Diff, Portfolio}
import com.wavesplatform.transaction.ValidationError.{GenericError, SenderIsBlacklisted}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.ReissueTransaction
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.{ScorexLogging, Time}
import kamon.Kamon
import kamon.metric.MeasurementUnit
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import monix.execution.schedulers.SchedulerService

import scala.collection.JavaConverters._
import scala.concurrent.duration.DurationLong
import scala.util.{Left, Right}

class UtxPoolImpl(time: Time, blockchain: Blockchain, fs: FunctionalitySettings, utxSettings: UtxSettings)
    extends ScorexLogging
    with Instrumented
    with AutoCloseable
    with UtxPool {
  outer =>

  import com.wavesplatform.utx.UtxPoolImpl._

  private implicit val scheduler: SchedulerService = Scheduler.singleThread("utx-pool-cleanup")

  private val transactions          = new ConcurrentHashMap[ByteStr, Transaction]()
  private val pessimisticPortfolios = new PessimisticPortfolios

  private val removeInvalidTask: Task[Unit] =
    Task.eval(removeInvalid()) >>
      Task.sleep(utxSettings.cleanupInterval) >>
      removeInvalidTask

  private val cleanup: CancelableFuture[Unit] = removeInvalidTask.runAsyncLogErr

  override def close(): Unit = {
    cleanup.cancel()
    scheduler.shutdown()
  }

  private val utxPoolSizeStats    = Kamon.rangeSampler("utx-pool-size", MeasurementUnit.none, Duration.of(500, ChronoUnit.MILLIS))
  private val processingTimeStats = Kamon.histogram("utx-transaction-processing-time", MeasurementUnit.time.milliseconds)
  private val putRequestStats     = Kamon.counter("utx-pool-put-if-new")

  private def removeExpired(currentTs: Long): Unit = {
    def isExpired(tx: Transaction) = (currentTs - tx.timestamp).millis > utxSettings.maxTransactionAge

    transactions.values.asScala
      .collect {
        case tx if isExpired(tx) => tx.id()
      }
      .foreach(remove)
  }

  override def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)] = putIfNew(blockchain, tx)

  private def checkNotBlacklisted(tx: Transaction): Either[ValidationError, Unit] = {
    if (utxSettings.blacklistSenderAddresses.isEmpty) {
      Right(())
    } else {
      val sender: Option[String] = tx match {
        case x: Authorized => Some(x.sender.address)
        case _             => None
      }

      sender match {
        case Some(addr) if utxSettings.blacklistSenderAddresses.contains(addr) =>
          val recipients = tx match {
            case tt: TransferTransaction      => Seq(tt.recipient)
            case mtt: MassTransferTransaction => mtt.transfers.map(_.address)
            case _                            => Seq()
          }
          val allowed =
            recipients.nonEmpty &&
              recipients.forall(r => utxSettings.allowBlacklistedTransferTo.contains(r.stringRepr))
          Either.cond(allowed, (), SenderIsBlacklisted(addr))
        case _ => Right(())
      }
    }
  }

  override def removeAll(txs: Traversable[Transaction]): Unit = {
    txs.view.map(_.id()).foreach(remove)
    removeExpired(time.correctedTime())
  }

  private def remove(txId: ByteStr): Unit = {
    Option(transactions.remove(txId)).foreach(_ => utxPoolSizeStats.decrement())
    pessimisticPortfolios.remove(txId)
  }

  private def removeInvalid(): Unit = {
    val b = blockchain
    val transactionsToRemove = transactions.values.asScala.filter { t =>
      TransactionDiffer(fs, b.lastBlockTimestamp, time.correctedTime(), b.height)(b, t).isLeft
    }
    removeAll(transactionsToRemove)
  }

  override def accountPortfolio(addr: Address): Portfolio = blockchain.portfolio(addr)

  override def portfolio(addr: Address): Portfolio =
    Monoid.combine(blockchain.portfolio(addr), pessimisticPortfolios.getAggregated(addr))

  override def all: Seq[Transaction] = transactions.values.asScala.toSeq.sorted(TransactionsOrdering.InUTXPool)

  override def size: Int = transactions.size

  override def transactionById(transactionId: ByteStr): Option[Transaction] = Option(transactions.get(transactionId))

  override def packUnconfirmed(rest: MultiDimensionalMiningConstraint): (Seq[Transaction], MultiDimensionalMiningConstraint) = {
    val currentTs = time.correctedTime()
    removeExpired(currentTs)
    val b      = blockchain
    val differ = TransactionDiffer(fs, blockchain.lastBlockTimestamp, currentTs, b.height) _
    val (invalidTxs, reversedValidTxs, _, finalConstraint, _) = transactions.values.asScala.toSeq
      .sorted(TransactionsOrdering.InUTXPool)
      .iterator
      .scanLeft((Seq.empty[ByteStr], Seq.empty[Transaction], Monoid[Diff].empty, rest, false)) {
        case ((invalid, valid, diff, currRest, isEmpty), tx) =>
          val updatedBlockchain = composite(b, diff)
          val updatedRest       = currRest.put(updatedBlockchain, tx)
          if (updatedRest.isOverfilled) {
            (invalid, valid, diff, currRest, isEmpty)
          } else {
            differ(updatedBlockchain, tx) match {
              case Right(newDiff) =>
                (invalid, tx +: valid, Monoid.combine(diff, newDiff), updatedRest, currRest.isEmpty)
              case Left(_) =>
                (tx.id() +: invalid, valid, diff, currRest, isEmpty)
            }
          }
      }
      .takeWhile(!_._5)
      .reduce((_, s) => s)

    invalidTxs.foreach(remove)
    val txs = reversedValidTxs.reverse
    (txs, finalConstraint)
  }

  override private[utx] def createBatchOps: UtxBatchOps = new BatchOpsImpl(blockchain)

  private class BatchOpsImpl(b: Blockchain) extends UtxBatchOps {
    override def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)] = outer.putIfNew(b, tx)
  }

  private def canReissue(b: Blockchain, tx: Transaction) = tx match {
    case r: ReissueTransaction if b.assetDescription(r.assetId).exists(!_.reissuable) => Left(GenericError(s"Asset is not reissuable"))
    case _                                                                            => Right(())
  }

  private def checkAlias(b: Blockchain, tx: Transaction) = tx match {
    case cat: CreateAliasTransaction if !blockchain.canCreateAlias(cat.alias) => Left(GenericError("Alias already claimed"))
    case _                                                                    => Right(())
  }

  private def checkScripted(b: Blockchain, tx: Transaction) =
    tx match {
      case a: AuthorizedTransaction if blockchain.hasScript(a.sender.toAddress) && (!utxSettings.allowTransactionsFromSmartAccounts) =>
        Left(GenericError("transactions from scripted accounts are denied from UTX pool"))
      case _ => Right(())
    }

  private def putIfNew(b: Blockchain, tx: Transaction): Either[ValidationError, (Boolean, Diff)] = {
    putRequestStats.increment()
    val result = measureSuccessful(
      processingTimeStats, {
        for {
          _    <- Either.cond(transactions.size < utxSettings.maxSize, (), GenericError("Transaction pool size limit is reached"))
          _    <- checkNotBlacklisted(tx)
          _    <- checkScripted(b, tx)
          _    <- checkAlias(b, tx)
          _    <- canReissue(b, tx)
          diff <- TransactionDiffer(fs, blockchain.lastBlockTimestamp, time.correctedTime(), blockchain.height)(b, tx)
        } yield {
          pessimisticPortfolios.add(tx.id(), diff)
          val isNew = Option(transactions.put(tx.id(), tx)).isEmpty
          if (isNew) utxPoolSizeStats.increment()
          (isNew, diff)
        }
      }
    )
    result.fold(
      err => log.trace(s"UTX putIfNew(${tx.id()}) failed with $err"),
      r => log.trace(s"UTX putIfNew(${tx.id()}) succeeded, isNew = ${r._1}")
    )
    result
  }
}

object UtxPoolImpl {

  private class PessimisticPortfolios {
    private type Portfolios = Map[Address, Portfolio]
    private val transactionPortfolios = new ConcurrentHashMap[ByteStr, Portfolios]()
    private val transactions          = new ConcurrentHashMap[Address, Set[ByteStr]]()

    def add(txId: ByteStr, txDiff: Diff): Unit = {
      val nonEmptyPessimisticPortfolios = txDiff.portfolios
        .map {
          case (addr, portfolio) => addr -> portfolio.pessimistic
        }
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
