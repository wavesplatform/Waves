package com.wavesplatform.utx

import java.time.Duration
import java.time.temporal.ChronoUnit
import java.util.concurrent.ConcurrentHashMap

import cats._
import com.google.common.collect.MapMaker
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.TransactionsOrdering
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.settings.{FunctionalitySettings, UtxSettings}
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.reader.CompositeBlockchain.composite
import com.wavesplatform.state.{Blockchain, Diff, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.ValidationError.{GenericError, SenderIsBlacklisted}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.ReissueTransaction
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.{ScorexLogging, Time}
import kamon.Kamon
import kamon.metric.MeasurementUnit
import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.{Observable, Observer}

import scala.collection.JavaConverters._
import scala.util.{Left, Right}

class UtxPoolImpl(time: Time,
                  blockchain: Blockchain,
                  spendableBalanceChanged: Observer[(Address, Asset)],
                  fs: FunctionalitySettings,
                  utxSettings: UtxSettings)
    extends ScorexLogging
    with Instrumented
    with AutoCloseable
    with UtxPool {
  outer =>

  import com.wavesplatform.utx.UtxPoolImpl._

  // State
  private[this] val transactions          = new ConcurrentHashMap[ByteStr, Transaction]()
  private[this] val pessimisticPortfolios = new PessimisticPortfolios(spendableBalanceChanged)

  // Metrics
  private[this] object PoolMetrics {
    private[this] val sizeStats         = Kamon.rangeSampler("utx-pool-size", MeasurementUnit.none, Duration.of(500, ChronoUnit.MILLIS))
    private[this] val scriptedSizeStats = Kamon.rangeSampler("utx-pool-scripted-size", MeasurementUnit.none, Duration.of(500, ChronoUnit.MILLIS))
    private[this] val bytesStats        = Kamon.rangeSampler("utx-pool-bytes", MeasurementUnit.information.bytes, Duration.of(500, ChronoUnit.MILLIS))
    val processingTimeStats             = Kamon.histogram("utx-transaction-processing-time", MeasurementUnit.time.milliseconds)
    val putRequestStats                 = Kamon.counter("utx-pool-put-if-new")

    def addTransaction(tx: Transaction): Unit = {
      sizeStats.increment()
      if (TxCheck.isScripted(tx)) scriptedSizeStats.increment()
      bytesStats.increment(tx.bytes().length)
    }

    def removeTransaction(tx: Transaction): Unit = {
      sizeStats.decrement()
      if (TxCheck.isScripted(tx)) scriptedSizeStats.decrement()
      bytesStats.decrement(tx.bytes().length)
    }
  }

  override def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)] = {
    def canReissue(blockchain: Blockchain, tx: Transaction) = tx match {
      case r: ReissueTransaction if !TxCheck.canReissue(r.asset) => Left(GenericError(s"Asset is not reissuable"))
      case _                                                     => Right(())
    }

    def checkAlias(blockchain: Blockchain, tx: Transaction) = tx match {
      case cat: CreateAliasTransaction if !TxCheck.canCreateAlias(cat.alias) => Left(GenericError("Alias already claimed"))
      case _                                                                 => Right(())
    }

    def checkScripted(blockchain: Blockchain, tx: Transaction, skipSizeCheck: Boolean) = tx match {
      case scripted if TxCheck.isScripted(scripted) =>
        for {
          _ <- Either.cond(utxSettings.allowTransactionsFromSmartAccounts,
                           (),
                           GenericError("transactions from scripted accounts are denied from UTX pool"))

          scriptedCount = transactions.values().asScala.count(TxCheck.isScripted)
          _ <- Either.cond(skipSizeCheck || scriptedCount < utxSettings.maxScriptedSize,
                           (),
                           GenericError("Transaction pool scripted txs size limit is reached"))
        } yield tx

      case _ =>
        Right(tx)
    }

    def checkNotBlacklisted(tx: Transaction) = {
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

    def checkIsMostProfitable(newTx: Transaction): Boolean = {
      transactions
        .values()
        .asScala
        .forall(poolTx => TransactionsOrdering.InUTXPool.compare(newTx, poolTx) < 0)
    }

    PoolMetrics.putRequestStats.increment()
    val result = measureSuccessful(
      PoolMetrics.processingTimeStats, {
        val skipSizeCheck = utxSettings.allowSkipChecks && checkIsMostProfitable(tx)

        for {
          _ <- Either.cond(skipSizeCheck || transactions.size < utxSettings.maxSize, (), GenericError("Transaction pool size limit is reached"))

          transactionsBytes = transactions.values.asScala // Bytes size of all transactions in pool
            .map(_.bytes().length)
            .sum
          _ <- Either.cond(skipSizeCheck || (transactionsBytes + tx.bytes().length) <= utxSettings.maxBytesSize,
                           (),
                           GenericError("Transaction pool bytes size limit is reached"))

          _    <- checkScripted(blockchain, tx, skipSizeCheck)
          _    <- checkNotBlacklisted(tx)
          _    <- checkAlias(blockchain, tx)
          _    <- canReissue(blockchain, tx)
          diff <- TransactionDiffer(fs, blockchain.lastBlockTimestamp, time.correctedTime(), blockchain.height)(blockchain, tx)
        } yield {
          pessimisticPortfolios.add(tx.id(), diff)
          val isNew = Option(transactions.put(tx.id(), tx)).isEmpty
          if (isNew) PoolMetrics.addTransaction(tx)
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

  override def removeAll(txs: Traversable[Transaction]): Unit = {
    txs.view.map(_.id()).foreach(remove)
    cleanup.doExpiredCleanup()
  }

  private[this] def afterRemove(tx: Transaction): Unit = {
    PoolMetrics.removeTransaction(tx)
    pessimisticPortfolios.remove(tx.id())
  }

  private[this] def remove(txId: ByteStr): Unit =
    Option(transactions.remove(txId))
      .foreach(afterRemove)

  override def spendableBalance(addr: Address, assetId: Asset): Long =
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
    cleanup.doExpiredCleanup()

    val differ = TransactionDiffer(fs, blockchain.lastBlockTimestamp, time.correctedTime(), blockchain.height) _
    val (invalidTxs, reversedValidTxs, _, finalConstraint, _) = transactions.values.asScala.toSeq
      .sorted(TransactionsOrdering.InUTXPool)
      .iterator
      .scanLeft((Seq.empty[ByteStr], Seq.empty[Transaction], Monoid[Diff].empty, rest, false)) {
        case ((invalid, valid, diff, currRest, isEmpty), tx) =>
          val updatedBlockchain = composite(blockchain, diff)
          val updatedRest       = currRest.put(updatedBlockchain, tx)
          if (updatedRest.isOverfilled) {
            log.trace(
              s"Mining constraints overfilled: ${MultiDimensionalMiningConstraint.formatOverfilledConstraints(currRest, updatedRest).mkString(", ")}")
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

  //noinspection ScalaStyle
  private[this] object TxCheck {
    private[this] val ExpirationTime = fs.maxTransactionTimeBackOffset.toMillis

    private[this] object Caches {
      val scriptedCache = {
        new MapMaker()
          .concurrencyLevel(Runtime.getRuntime.availableProcessors())
          .weakKeys()
          .makeMap[Address, Boolean]()
          .asScala
      }

      val createAliasCache = {
        new MapMaker()
          .concurrencyLevel(Runtime.getRuntime.availableProcessors())
          .weakKeys()
          .makeMap[String, Boolean]()
          .asScala
      }

      val reissueCache = {
        new MapMaker()
          .concurrencyLevel(Runtime.getRuntime.availableProcessors())
          .weakKeys()
          .makeMap[ByteStr, Boolean]()
          .asScala
      }

      def clear(): Unit = {
        scriptedCache.clear()
        createAliasCache.clear()
        reissueCache.clear()
      }
    }

    def isExpired(transaction: Transaction, currentTime: Long = time.correctedTime()): Boolean = {
      (currentTime - transaction.timestamp) > ExpirationTime
    }

    def isValid(transaction: Transaction,
                lastBlockTimestamp: Option[Long] = blockchain.lastBlockTimestamp,
                currentTime: Long = time.correctedTime(),
                height: Int = blockchain.height): Boolean = {
      !isExpired(transaction) && TransactionDiffer(fs, lastBlockTimestamp, currentTime, height)(blockchain, transaction).isRight
    }

    def isScripted(transaction: Transaction): Boolean = {
      transaction match {
        case a: AuthorizedTransaction => Caches.scriptedCache.getOrElseUpdate(a.sender.toAddress, blockchain.hasScript(a.sender.toAddress))
        case _                        => false
      }
    }

    def canCreateAlias(alias: Alias): Boolean = {
      Caches.createAliasCache.getOrElseUpdate(alias.name, blockchain.canCreateAlias(alias))
    }

    def canReissue(asset: IssuedAsset): Boolean = {
      Caches.reissueCache.getOrElseUpdate(asset.id, blockchain.assetDescription(asset).forall(_.reissuable))
    }

    def clearCaches(): Unit = {
      Caches.clear()
    }
  }

  //noinspection ScalaStyle
  object cleanup {
    private[UtxPoolImpl] implicit val scheduler: SchedulerService = Scheduler.singleThread("utx-pool-cleanup")

    val runCleanupTask: Task[Unit] = Task
      .eval(doCleanup())
      .executeOn(scheduler)

    def runCleanupOn(observable: Observable[_]): Cancelable = {
      observable
        .whileBusyDropEventsAndSignal(dropped => log.warn(s"UTX pool cleanup is too slow, $dropped cleanups skipped"))
        .mapTask(_ => runCleanupTask)
        .doOnComplete(() => log.debug("UTX pool cleanup stopped"))
        .doOnError(err => log.error("UTX pool cleanup error", err))
        .subscribe()
    }

    private[UtxPoolImpl] def doExpiredCleanup(): Unit = {
      transactions.entrySet().removeIf { entry =>
        val tx     = entry.getValue
        val remove = TxCheck.isExpired(tx)
        if (remove) UtxPoolImpl.this.afterRemove(tx)
        remove
      }
    }

    private[UtxPoolImpl] def doCleanup(): Unit = {
      transactions.entrySet().removeIf { entry =>
        val tx     = entry.getValue
        val remove = !TxCheck.isValid(tx)
        if (remove) UtxPoolImpl.this.afterRemove(tx)
        remove
      }

      TxCheck.clearCaches()
    }
  }

  override def close(): Unit = {
    cleanup.scheduler.shutdown()
  }
}

object UtxPoolImpl {
  private class PessimisticPortfolios(spendableBalanceChanged: Observer[(Address, Asset)]) {
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
