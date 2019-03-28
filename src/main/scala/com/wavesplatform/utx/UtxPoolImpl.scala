package com.wavesplatform.utx

import java.time.Duration
import java.time.temporal.ChronoUnit
import java.util.concurrent.ConcurrentHashMap

import cats._
import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.TransactionsOrdering
import com.wavesplatform.metrics._
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.settings.{FunctionalitySettings, UtxSettings}
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.reader.CompositeBlockchain.composite
import com.wavesplatform.state.{Blockchain, Diff, Portfolio}
import com.wavesplatform.transaction.ValidationError.{GenericError, SenderIsBlacklisted}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.ReissueTransaction
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.{ScorexLogging, Time}
import kamon.Kamon
import kamon.metric.MeasurementUnit
import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observer

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.{Left, Right}

class UtxPoolImpl(time: Time,
                  blockchain: Blockchain,
                  spendableBalanceChanged: Observer[(Address, Option[AssetId])],
                  fs: FunctionalitySettings,
                  utxSettings: UtxSettings)
    extends ScorexLogging
    with AutoCloseable
    with UtxPool {
  outer =>

  import com.wavesplatform.utx.UtxPoolImpl._

  private implicit val scheduler: SchedulerService = Scheduler.singleThread("utx-pool-cleanup")

  private val transactions          = new ConcurrentHashMap[ByteStr, Transaction]()
  private val pessimisticPortfolios = new PessimisticPortfolios(spendableBalanceChanged)

  private val removeInvalidTask: Task[Unit] =
    Task.eval(removeInvalid()) >>
      Task.sleep(utxSettings.cleanupInterval) >>
      removeInvalidTask

  private val cleanup: CancelableFuture[Unit] = removeInvalidTask.runAsyncLogErr

  override def close(): Unit = {
    cleanup.cancel()
    scheduler.shutdown()
  }

  private def removeExpired(currentTs: Long): Unit = {
    def isExpired(tx: Transaction) = (currentTs - tx.timestamp).millis > fs.maxTransactionTimeBackOffset

    val toRemove = transactions.values.asScala
      .collect { case tx if isExpired(tx) => tx.id() }

    if (toRemove.nonEmpty) {
      log.trace(s"Removing expired transactions: [${toRemove.mkString(", ")}]")
      toRemove.foreach(remove)
    }
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
    Option(transactions.remove(txId)).foreach(poolMetrics.removeTransaction)
    pessimisticPortfolios.remove(txId)
  }

  private def removeInvalid(): Unit = {
    val b = blockchain
    val transactionsToRemove = transactions.values.asScala.filter { t =>
      TransactionDiffer(fs, b.lastBlockTimestamp, time.correctedTime(), b.height)(b, t).isLeft
    }

    if (transactionsToRemove.nonEmpty) log.trace(s"Cleaning up invalid transactions: [${transactionsToRemove.mkString(", ")}]")
    removeAll(transactionsToRemove)
  }

  override def spendableBalance(addr: Address, assetId: Option[AssetId]): Long =
    blockchain.balance(addr, assetId) -
      assetId.fold(blockchain.leaseBalance(addr).out)(_ => 0L) +
      pessimisticPortfolios
        .getAggregated(addr)
        .spendableBalanceOf(assetId)

  override def pessimisticPortfolio(addr: Address): Portfolio = pessimisticPortfolios.getAggregated(addr)

  override def all: Seq[Transaction] = transactions.values.asScala.toSeq.sorted(TransactionsOrdering.InUTXPool)

  override def size: Int = transactions.size

  override def transactionById(transactionId: ByteStr): Option[Transaction] = Option(transactions.get(transactionId))

  override def packUnconfirmed(rest: MultiDimensionalMiningConstraint): (Seq[Transaction], MultiDimensionalMiningConstraint) = {
    val currentTs = time.correctedTime()
    removeExpired(currentTs)
    val b = blockchain

    val (invalidTxs, reversedValidTxs, _, finalConstraint, _) = poolMetrics.packTimeStats.measure {
      val differ = TransactionDiffer(fs, blockchain.lastBlockTimestamp, currentTs, b.height) _
      transactions.values.asScala.toSeq
        .sorted(TransactionsOrdering.InUTXPool)
        .iterator
        .scanLeft((Seq.empty[ByteStr], Seq.empty[Transaction], Monoid[Diff].empty, rest, false)) {
          case ((invalid, valid, diff, currRest, _), tx) =>
            val updatedBlockchain = composite(b, diff)
            val updatedRest       = currRest.put(updatedBlockchain, tx)
            if (updatedRest.isOverfilled) {
              (invalid, valid, diff, currRest, currRest.isEmpty)
            } else {
              differ(updatedBlockchain, tx) match {
                case Right(newDiff) =>
                  (invalid, tx +: valid, Monoid.combine(diff, newDiff), updatedRest, currRest.isEmpty)
                case Left(_) =>
                  (tx.id() +: invalid, valid, diff, currRest, currRest.isEmpty)
              }
            }
        }
        .takeWhile(!_._5) // !currRest.isEmpty
        .reduce((_, right) => right)
    }

    if (invalidTxs.nonEmpty) {
      log.trace(s"Removing ${invalidTxs.length} invalid transactions from UTX: [${invalidTxs.mkString(", ")}]")
      invalidTxs.foreach(remove)
    }

    val txs = reversedValidTxs.reverse
    if (txs.nonEmpty) log.trace(s"Packed ${txs.length} unconfirmed transactions, final constraint = $finalConstraint")
    (txs, finalConstraint)
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
    poolMetrics.putRequestStats.increment()
    val result = poolMetrics.putTimeStats.measureSuccessful(for {
      _ <- Either.cond(transactions.size < utxSettings.maxSize, (), GenericError("Transaction pool size limit is reached"))

      transactionsBytes = transactions.values.asScala // Bytes size of all transactions in pool
        .map(_.bytes().length)
        .sum
      _ <- Either.cond((transactionsBytes + tx.bytes().length) <= utxSettings.maxBytesSize,
        (),
        GenericError("Transaction pool bytes size limit is reached"))

      _    <- checkNotBlacklisted(tx)
      _    <- checkScripted(b, tx)
      _    <- checkAlias(b, tx)
      _    <- canReissue(b, tx)
      diff <- TransactionDiffer(fs, blockchain.lastBlockTimestamp, time.correctedTime(), blockchain.height)(b, tx)
    } yield {
      pessimisticPortfolios.add(tx.id(), diff)
      val isNew = Option(transactions.put(tx.id(), tx)).isEmpty
      if (isNew) poolMetrics.addTransaction(tx)
      (isNew, diff)
    })

    result.fold(
      err => log.trace(s"UTX putIfNew(${tx.id()}) failed with $err"),
      r => log.trace(s"UTX putIfNew(${tx.id()}) succeeded, isNew = ${r._1}")
    )

    result
  }

  private[this] object poolMetrics {
    private[this] val sizeStats  = Kamon.rangeSampler("utx.pool-size", MeasurementUnit.none, Duration.of(500, ChronoUnit.MILLIS))
    private[this] val bytesStats = Kamon.rangeSampler("utx.pool-bytes", MeasurementUnit.information.bytes, Duration.of(500, ChronoUnit.MILLIS))
    val putTimeStats             = Kamon.timer("utx.put-if-new")
    val putRequestStats          = Kamon.counter("utx.put-if-new.requests")
    val packTimeStats            = Kamon.timer("utx.pack-unconfirmed")

    def addTransaction(tx: Transaction): Unit = {
      sizeStats.increment()
      bytesStats.increment(tx.bytes().length)
    }

    def removeTransaction(tx: Transaction): Unit = {
      sizeStats.decrement()
      bytesStats.decrement(tx.bytes().length)
    }
  }
}

object UtxPoolImpl {

  private class PessimisticPortfolios(spendableBalanceChanged: Observer[(Address, Option[AssetId])]) {
    private type Portfolios = Map[Address, Portfolio]
    private val transactionPortfolios = new ConcurrentHashMap[ByteStr, Portfolios]()
    private val transactions          = new ConcurrentHashMap[Address, Set[ByteStr]]()

    def add(txId: ByteStr, txDiff: Diff): Unit = {
      val pessimisticPortfolios         = txDiff.portfolios.map { case (addr, portfolio)        => addr -> portfolio.pessimistic }
      val nonEmptyPessimisticPortfolios = pessimisticPortfolios.filterNot { case (_, portfolio) => portfolio.isEmpty }

      if (nonEmptyPessimisticPortfolios.nonEmpty &&
          Option(transactionPortfolios.put(txId, nonEmptyPessimisticPortfolios)).isEmpty) {
        nonEmptyPessimisticPortfolios.keys.foreach { address =>
          transactions.put(address, transactions.getOrDefault(address, Set.empty) + txId)
        }
      }

      // Because we need to notify about balance changes when they are applied
      pessimisticPortfolios.foreach {
        case (addr, p) => p.assetIds.foreach(assetId => spendableBalanceChanged.onNext(addr -> assetId))
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
      Option(transactionPortfolios.remove(txId)) match {
        case Some(txPortfolios) =>
          txPortfolios.foreach {
            case (addr, p) =>
              transactions.computeIfPresent(addr, (_, prevTxs) => prevTxs - txId)
              p.assetIds.foreach(assetId => spendableBalanceChanged.onNext(addr -> assetId))
          }
        case None =>
      }
    }
  }

}
