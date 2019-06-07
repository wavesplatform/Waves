package com.wavesplatform.utx

import java.time.Duration
import java.time.temporal.ChronoUnit
import java.util.concurrent.ConcurrentHashMap

import cats._
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.TransactionsOrdering
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics._
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.settings.UtxSettings
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.reader.CompositeBlockchain.composite
import com.wavesplatform.state.{Blockchain, Diff, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.{GenericError, SenderIsBlacklisted}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.ReissueTransaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.{ScorexLogging, Time}
import kamon.Kamon
import kamon.metric.MeasurementUnit
import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.{Observable, Observer}

import scala.collection.JavaConverters._
import scala.concurrent.duration.{Duration => ScalaDuration}
import scala.util.{Left, Right}

class UtxPoolImpl(time: Time, blockchain: Blockchain, spendableBalanceChanged: Observer[(Address, Asset)], utxSettings: UtxSettings)
    extends ScorexLogging
    with AutoCloseable
    with UtxPool {
  outer =>

  import com.wavesplatform.utx.UtxPoolImpl._

  // State
  private[this] val transactions          = new ConcurrentHashMap[ByteStr, Transaction]()
  private[this] val pessimisticPortfolios = new PessimisticPortfolios(spendableBalanceChanged)

  override def putIfNew(tx: Transaction, verify: Boolean): TracedResult[ValidationError, Boolean] = {
    if (transactions.containsKey(tx.id())) TracedResult.wrapValue(false)
    else putNewTx(tx, verify)
  }

  protected def putNewTx(tx: Transaction, verify: Boolean): TracedResult[ValidationError, Boolean] = {
    PoolMetrics.putRequestStats.increment()

    val checks = if (verify) PoolMetrics.putTimeStats.measure {
      object LimitChecks {
        def canReissue(tx: Transaction): Either[GenericError, Unit] =
          PoolMetrics.checkCanReissue.measure(tx match {
            case r: ReissueTransaction if !TxCheck.canReissue(r.asset) => Left(GenericError(s"Asset is not reissuable"))
            case _ => Right(())
          })

        def checkAlias(tx: Transaction): Either[GenericError, Unit] =
          PoolMetrics.checkAlias.measure(tx match {
            case cat: CreateAliasTransaction if !TxCheck.canCreateAlias(cat.alias) => Left(GenericError("Alias already claimed"))
            case _ => Right(())
          })

        def checkScripted(tx: Transaction, skipSizeCheck: Boolean): Either[GenericError, Transaction] =
          PoolMetrics.checkScripted.measure(tx match {
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
          })

        def checkNotBlacklisted(tx: Transaction): Either[SenderIsBlacklisted, Unit] = PoolMetrics.checkNotBlacklisted.measure {
          if (utxSettings.blacklistSenderAddresses.isEmpty) {
            Right(())
          } else {
            val sender: Option[String] = tx match {
              case x: Authorized => Some(x.sender.address)
              case _ => None
            }

            sender match {
              case Some(addr) if utxSettings.blacklistSenderAddresses.contains(addr) =>
                val recipients = tx match {
                  case tt: TransferTransaction => Seq(tt.recipient)
                  case mtt: MassTransferTransaction => mtt.transfers.map(_.address)
                  case _ => Seq()
                }
                val allowed =
                  recipients.nonEmpty &&
                    recipients.forall(r => utxSettings.allowBlacklistedTransferTo.contains(r.stringRepr))
                Either.cond(allowed, (), SenderIsBlacklisted(addr))
              case _ => Right(())
            }
          }
        }

        def checkIsMostProfitable(newTx: Transaction): Boolean = PoolMetrics.checkIsMostProfitable.measure {
          transactions
            .values()
            .asScala
            .forall(poolTx => TransactionsOrdering.InUTXPool.compare(newTx, poolTx) < 0)
        }
      }

      lazy val skipSizeCheck = utxSettings.allowSkipChecks && LimitChecks.checkIsMostProfitable(tx)
      lazy val transactionsBytes = transactions.values.asScala // Bytes size of all transactions in pool
        .map(_.bytes().length)
        .sum

      for {
        _ <- Either.cond(transactions.size < utxSettings.maxSize || skipSizeCheck, (), GenericError("Transaction pool size limit is reached"))
        _ <- Either.cond(skipSizeCheck || (transactionsBytes + tx.bytes().length) <= utxSettings.maxBytesSize,
                         (),
                         GenericError("Transaction pool bytes size limit is reached"))

        _ <- LimitChecks.checkScripted(tx, skipSizeCheck)
        _ <- LimitChecks.checkNotBlacklisted(tx)
        _ <- LimitChecks.checkAlias(tx)
        _ <- LimitChecks.canReissue(tx)
      } yield ()
    } else Right(())

    val tracedIsNew = TracedResult(checks).flatMap(_ => addTransaction(tx, verify))
    tracedIsNew.resultE match {
      case Left(err) => log.trace(s"UTX putIfNew(${tx.id()}) failed with $err, trace = ${tracedIsNew.trace}")
      case Right(_) => log.trace(s"UTX putIfNew(${tx.id()}) succeeded, isNew = true, trace = ${tracedIsNew.trace}")
    }
    tracedIsNew
  }

  override def removeAll(txs: Traversable[Transaction]): Unit =
    txs.view
      .map(_.id())
      .foreach(remove)

  private[this] def afterRemove(tx: Transaction): Unit = {
    PoolMetrics.removeTransaction(tx)
    pessimisticPortfolios.remove(tx.id())
  }

  private[this] def remove(txId: ByteStr): Unit =
    Option(transactions.remove(txId))
      .foreach(afterRemove)

  private[this] def addTransaction(tx: Transaction, verify: Boolean): TracedResult[ValidationError, Boolean] = {
    val isNew = TransactionDiffer(blockchain.lastBlockTimestamp, time.correctedTime(), blockchain.height, verify)(blockchain, tx)
      .map { diff => pessimisticPortfolios.add(tx.id(), diff); true }

    if (!verify || isNew.resultE.isRight) {
      transactions.put(tx.id(), tx)
      PoolMetrics.addTransaction(tx)
    }

    isNew
  }

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

  override def packUnconfirmed(rest: MultiDimensionalMiningConstraint,
                               maxPackTime: ScalaDuration): (Seq[Transaction], MultiDimensionalMiningConstraint) = {
    val differ = TransactionDiffer(blockchain.lastBlockTimestamp, time.correctedTime(), blockchain.height) _
    val (reversedValidTxs, _, finalConstraint, _, _, totalIterations) = PoolMetrics.packTimeStats.measure {
      val startTime                   = System.nanoTime()
      def isTimeLimitReached: Boolean = maxPackTime.isFinite() && (System.nanoTime() - startTime) >= maxPackTime.toNanos

      transactions.values.asScala.toSeq
        .sorted(TransactionsOrdering.InUTXPool)
        .iterator
        .scanLeft((Seq.empty[Transaction], Monoid[Diff].empty, rest, false, rest, 0)) {
          case ((valid, diff, currRest, _, lastOverfilled, iterations), tx) =>
            val preUpdatedRest = currRest.put(blockchain, tx, Diff.empty) // TODO: Doesn't handle scriptRuns/scriptComplexity
            if (preUpdatedRest.isOverfilled) {
              if (preUpdatedRest != lastOverfilled) {
                log.trace(
                  s"Mining constraints overfilled with $tx: ${MultiDimensionalMiningConstraint.formatOverfilledConstraints(currRest, preUpdatedRest).mkString(", ")}")
              }
              (valid, diff, currRest, currRest.isEmpty, preUpdatedRest, iterations + 1)
            } else {
              val updatedBlockchain = composite(blockchain, diff)
              if (TxCheck.isExpired(tx)) {
                log.trace(s"Transaction [${tx.id()}] is expired")
                remove(tx.id())
                (valid, diff, currRest, currRest.isEmpty, lastOverfilled, iterations + 1)
              } else {
                differ(updatedBlockchain, tx).resultE match {
                  case Right(newDiff) =>
                    val updatedRest = currRest.put(updatedBlockchain, tx, newDiff)
                    if (updatedRest.isOverfilled) {
                      if (updatedRest != lastOverfilled) {
                        log.trace(
                          s"Mining constraints overfilled with $tx: ${MultiDimensionalMiningConstraint.formatOverfilledConstraints(currRest, updatedRest).mkString(", ")}")
                      }
                      (valid, diff, currRest, currRest.isEmpty, updatedRest, iterations + 1)
                    } else {
                      (tx +: valid, Monoid.combine(diff, newDiff), updatedRest, currRest.isEmpty, lastOverfilled, iterations + 1)
                    }
                  case Left(error) =>
                    log.trace(s"Transaction [${tx.id()}] is removed: $error")
                    remove(tx.id())
                    (valid, diff, currRest, currRest.isEmpty, lastOverfilled, iterations + 1)
                }
              }
            }
        }
        .takeWhile(r => !r._4 && (r._1.isEmpty || !isTimeLimitReached)) // !currRest.isEmpty && (validTxs.isEmpty || !isTimeLimitReached)
        .reduce((_, right) => right)
    }

    val txs = reversedValidTxs.reverse
    if (txs.nonEmpty) log.trace(s"Packed ${txs.length} transactions of $totalIterations checked, final constraint: $finalConstraint")
    (txs, finalConstraint)
  }

  //noinspection ScalaStyle
  private[this] object TxCheck {
    private[this] val ExpirationTime = blockchain.settings.functionalitySettings.maxTransactionTimeBackOffset.toMillis

    def isExpired(transaction: Transaction, currentTime: Long = time.correctedTime()): Boolean = {
      (currentTime - transaction.timestamp) > ExpirationTime
    }

    def validate(transaction: Transaction,
                 lastBlockTimestamp: Option[Long] = blockchain.lastBlockTimestamp,
                 currentTime: Long = time.correctedTime(),
                 height: Int = blockchain.height): Either[ValidationError, Diff] = {
      for {
        _    <- Either.cond(!isExpired(transaction), (), GenericError("Transaction is expired"))
        diff <- TransactionDiffer(lastBlockTimestamp, currentTime, height)(blockchain, transaction).resultE
      } yield diff
    }

    def isScripted(transaction: Transaction): Boolean = {
      transaction match {
        case a: AuthorizedTransaction => blockchain.hasScript(a.sender.toAddress)
        case _                        => false
      }
    }

    def canCreateAlias(alias: Alias): Boolean =
      blockchain.canCreateAlias(alias)

    def canReissue(asset: IssuedAsset): Boolean =
      blockchain.assetDescription(asset).forall(_.reissuable)
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

    private[UtxPoolImpl] def doCleanup(): Unit = {
      UtxPoolImpl.this.packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, ScalaDuration.Inf)
    }
  }

  override def close(): Unit = {
    cleanup.scheduler.shutdown()
  }

  private[this] object PoolMetrics {
    private[this] val sizeStats  = Kamon.rangeSampler("utx.pool-size", MeasurementUnit.none, Duration.of(500, ChronoUnit.MILLIS))
    private[this] val bytesStats = Kamon.rangeSampler("utx.pool-bytes", MeasurementUnit.information.bytes, Duration.of(500, ChronoUnit.MILLIS))
    val putTimeStats             = Kamon.timer("utx.put-if-new")
    val putRequestStats          = Kamon.counter("utx.put-if-new.requests")
    val packTimeStats            = Kamon.timer("utx.pack-unconfirmed")

    val checkIsMostProfitable = Kamon.timer("utx.check.is-most-profitable")
    val checkAlias            = Kamon.timer("utx.check.alias")
    val checkCanReissue       = Kamon.timer("utx.check.can-reissue")
    val checkNotBlacklisted   = Kamon.timer("utx.check.not-blacklisted")
    val checkScripted         = Kamon.timer("utx.check.scripted")

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

    def contains(txId: ByteStr): Boolean = transactionPortfolios.containsKey(txId)

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
