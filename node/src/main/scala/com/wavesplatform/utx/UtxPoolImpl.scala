package com.wavesplatform.utx

import java.time.Duration
import java.time.temporal.ChronoUnit
import java.util.concurrent.ConcurrentHashMap

import cats.Monoid
import cats.syntax.monoid._
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.TransactionsOrdering
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics._
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.settings.UtxSettings
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{Blockchain, Diff, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.{AlreadyInTheState, GenericError, SenderIsBlacklisted}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.ReissueTransaction
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.{LoggerFacade, Schedulers, ScorexLogging, Time}
import kamon.Kamon
import kamon.metric.MeasurementUnit
import monix.execution.atomic.AtomicBoolean
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observer
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.duration.{Duration => ScalaDuration}
import scala.util.{Left, Right}

class UtxPoolImpl(
    time: Time,
    blockchain: Blockchain,
    spendableBalanceChanged: Observer[(Address, Asset)],
    utxSettings: UtxSettings,
    nanoTimeSource: () => Long = () => System.nanoTime(),
    enablePriorityPool: Boolean
) extends ScorexLogging
    with AutoCloseable
    with UtxPool {

  import com.wavesplatform.utx.UtxPoolImpl._

  // Context
  private[this] val cleanupScheduler: SchedulerService = Schedulers.singleThread("utx-pool-cleanup")

  // State
  private[this] val transactions          = new ConcurrentHashMap[ByteStr, Transaction]()
  private[this] val pessimisticPortfolios = new PessimisticPortfolios(spendableBalanceChanged, blockchain.transactionHeight(_).nonEmpty)
  private[this] val priorityTransactions  = mutable.LinkedHashMap.empty[ByteStr, Transaction]

  override def putIfNew(tx: Transaction): TracedResult[ValidationError, Boolean] = {
    if (transactions.containsKey(tx.id()) || priorityTransactions.contains(tx.id())) TracedResult.wrapValue(false)
    else putNewTx(tx, verify = true)
  }

  private[this] def putNewTx(tx: Transaction, verify: Boolean): TracedResult[ValidationError, Boolean] = {
    PoolMetrics.putRequestStats.increment()

    val checks = if (verify) PoolMetrics.putTimeStats.measure {
      object LimitChecks {
        def canReissue(tx: Transaction): Either[GenericError, Unit] =
          PoolMetrics.checkCanReissue.measure(tx match {
            case r: ReissueTransaction if !TxCheck.canReissue(r.asset) => Left(GenericError(s"Asset is not reissuable"))
            case _                                                     => Right(())
          })

        def checkAlias(tx: Transaction): Either[GenericError, Unit] =
          PoolMetrics.checkAlias.measure(tx match {
            case cat: CreateAliasTransaction if !TxCheck.canCreateAlias(cat.alias) => Left(GenericError("Alias already claimed"))
            case _                                                                 => Right(())
          })

        def checkScripted(tx: Transaction, skipSizeCheck: Boolean): Either[GenericError, Transaction] =
          PoolMetrics.checkScripted.measure(
            if (!TxCheck.isScripted(tx)) Right(tx)
            else
              for {
                _ <- Either.cond(
                  utxSettings.allowTransactionsFromSmartAccounts,
                  (),
                  GenericError("transactions from scripted accounts are denied from UTX pool")
                )
                _ <- Either.cond(
                  skipSizeCheck || transactions.values().asScala.count(TxCheck.isScripted) < utxSettings.maxScriptedSize,
                  (),
                  GenericError("Transaction pool scripted txs size limit is reached")
                )
              } yield tx
          )

        def checkNotBlacklisted(tx: Transaction): Either[SenderIsBlacklisted, Unit] = PoolMetrics.checkNotBlacklisted.measure {
          if (utxSettings.blacklistSenderAddresses.isEmpty) {
            Right(())
          } else {
            val sender: Option[String] = tx match {
              case x: Authorized => Some(x.sender.stringRepr)
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
        _ <- Either.cond(skipSizeCheck || transactions.size < utxSettings.maxSize, (), GenericError("Transaction pool size limit is reached"))
        _ <- Either.cond(
          skipSizeCheck || (transactionsBytes + tx.bytes().length) <= utxSettings.maxBytesSize,
          (),
          GenericError("Transaction pool bytes size limit is reached")
        )

        _ <- LimitChecks.checkNotBlacklisted(tx)
        _ <- LimitChecks.checkScripted(tx, skipSizeCheck)
        _ <- LimitChecks.checkAlias(tx)
        _ <- LimitChecks.canReissue(tx)
      } yield ()
    } else Right(())

    val tracedIsNew = TracedResult(checks).flatMap(_ => addTransaction(tx, verify, priority = false))
    tracedIsNew.resultE match {
      case Right(isNew) => log.trace(s"UTX putIfNew(${tx.id()}) succeeded, isNew = $isNew")
      case Left(err) =>
        log.debug(s"UTX putIfNew(${tx.id()}) failed with ${extractErrorMessage(err)}")
        traceLogger.trace(err.toString)
    }
    tracedIsNew
  }

  override def removeAll(txs: Traversable[Transaction]): Unit = {
    val ids = txs.map(_.id()).toSet
    removeIds(ids)
  }

  private[this] def removeFromBothPools(txId: ByteStr): Unit = priorityTransactions.synchronized {
    val removedFromPriority = priorityTransactions.remove(txId)
    removedFromPriority.foreach(PoolMetrics.removeTransactionPriority)
    removeFromOrdPool(txId)
  }

  private[this] def removeFromOrdPool(txId: ByteStr): Unit = priorityTransactions.synchronized {
    for (tx <- Option(transactions.remove(txId))) PoolMetrics.removeTransaction(tx)
    if (!priorityTransactions.contains(txId)) pessimisticPortfolios.remove(txId)
  }

  private[this] def removeIds(removed: Set[ByteStr]): Unit = priorityTransactions.synchronized {
    removed.foreach(removeFromBothPools)
  }

  private[this] def addTransaction(tx: Transaction, verify: Boolean, priority: Boolean): TracedResult[ValidationError, Boolean] = {
    val diffEi               = TransactionDiffer(blockchain.lastBlockTimestamp, time.correctedTime(), blockchain.height, verify)(blockchain, tx)
    def addPortfolio(): Unit = diffEi.map(pessimisticPortfolios.add(tx.id(), _))

    if (!verify || diffEi.resultE.isRight) {
      if (priority) priorityTransactions.synchronized {
        priorityTransactions += tx.id() -> tx
        PoolMetrics.addTransactionPriority(tx)
        addPortfolio()
      } else
        transactions.computeIfAbsent(tx.id(), { _ =>
          PoolMetrics.addTransaction(tx)
          addPortfolio()
          tx
        })
    }

    diffEi.map(_ => true)
  }

  override def spendableBalance(addr: Address, assetId: Asset): Long =
    blockchain.balance(addr, assetId) -
      assetId.fold(blockchain.leaseBalance(addr).out)(_ => 0L) +
      pessimisticPortfolios
        .getAggregated(addr)
        .spendableBalanceOf(assetId)

  override def pessimisticPortfolio(addr: Address): Portfolio =
    pessimisticPortfolios.getAggregated(addr)

  private[this] def nonPriorityTransactions: Seq[Transaction] = {
    transactions.values.asScala.toVector
      .sorted(TransactionsOrdering.InUTXPool)
  }

  override def all: Seq[Transaction] =
    (priorityTransactions.values ++ nonPriorityTransactions).toVector.distinct

  override def size: Int = transactions.size

  override def transactionById(transactionId: ByteStr): Option[Transaction] = Option(transactions.get(transactionId))

  private def scriptedAddresses(tx: Transaction): Set[Address] = tx match {
    case i: InvokeScriptTransaction =>
      Set(i.sender.toAddress).filter(blockchain.hasScript) ++ blockchain.resolveAlias(i.dAppAddressOrAlias).fold[Set[Address]](_ => Set.empty, Set(_))
    case e: ExchangeTransaction =>
      Set(e.sender.toAddress, e.buyOrder.sender.toAddress, e.sellOrder.sender.toAddress).filter(blockchain.hasScript)
    case a: Authorized if blockchain.hasScript(a.sender.toAddress) => Set(a.sender.toAddress)
    case _                                                         => Set.empty
  }

  private[this] case class TxEntry(tx: Transaction, priority: Boolean)

  private[this] def createTxEntrySeq(): Seq[TxEntry] =
    priorityTransactions.synchronized(priorityTransactions.values.map(TxEntry(_, priority = true)).toVector) ++ nonPriorityTransactions.map(
      TxEntry(_, priority = false)
    )

  override def packUnconfirmed(
      initialConstraint: MultiDimensionalMiningConstraint,
      maxPackTime: ScalaDuration
  ): (Option[Seq[Transaction]], MultiDimensionalMiningConstraint) = {

    val differ = TransactionDiffer(blockchain.lastBlockTimestamp, time.correctedTime(), blockchain.height) _
    val packResult = PoolMetrics.packTimeStats.measure {
      val startTime                   = nanoTimeSource()
      def isTimeLimitReached: Boolean = maxPackTime.isFinite() && (nanoTimeSource() - startTime) >= maxPackTime.toNanos

      def packIteration(prevResult: PackResult, sortedTransactions: Iterator[TxEntry]): PackResult =
        sortedTransactions
          .filterNot(e => prevResult.validatedTransactions(e.tx.id()))
          .foldLeft[PackResult](prevResult) {
            case (r, TxEntry(tx, priority)) =>
              def isLimitReached   = r.transactions.exists(_.nonEmpty) && isTimeLimitReached
              def isAlreadyRemoved = !priority && !transactions.containsKey(tx.id())

              if (r.constraint.isFull || isLimitReached || isAlreadyRemoved)
                r // don't run any checks here to speed up mining
              else if (TxCheck.isExpired(tx)) {
                log.debug(s"Transaction ${tx.id()} expired")
                this.removeFromBothPools(tx.id())
                r.copy(iterations = r.iterations + 1)
              } else {
                val newScriptedAddresses = scriptedAddresses(tx)
                if (!priority && r.checkedAddresses.intersect(newScriptedAddresses).nonEmpty) r
                else {
                  val updatedBlockchain   = CompositeBlockchain(blockchain, Some(r.totalDiff))
                  val newCheckedAddresses = newScriptedAddresses ++ r.checkedAddresses
                  differ(updatedBlockchain, tx).resultE match {
                    case Right(newDiff) =>
                      val updatedConstraint = r.constraint.put(updatedBlockchain, tx, newDiff)
                      if (updatedConstraint.isOverfilled) {
                        log.trace(
                          s"Transaction ${tx.id()} does not fit into the block: " +
                            s"${MultiDimensionalMiningConstraint.formatOverfilledConstraints(r.constraint, updatedConstraint).mkString(", ")}"
                        )
                        r.copy(
                          transactions = r.transactions.orElse(Some(Seq.empty[Transaction])),
                          iterations = r.iterations + 1,
                          checkedAddresses = newCheckedAddresses,
                          validatedTransactions = r.validatedTransactions + tx.id()
                        )
                      } else {
                        log.trace(s"Packing transaction ${tx.id()}")
                        PackResult(
                          Some(r.transactions.fold(Seq(tx))(tx +: _)),
                          r.totalDiff.combine(newDiff),
                          updatedConstraint,
                          r.iterations + 1,
                          newCheckedAddresses,
                          r.validatedTransactions + tx.id()
                        )
                      }

                    case Left(TransactionValidationError(AlreadyInTheState(txId, _), tx)) if r.validatedTransactions.contains(tx.id()) =>
                      log.trace(s"Transaction $txId already validated in priority pool")
                      removeFromOrdPool(tx.id())
                      r

                    case Left(error) =>
                      log.debug(s"Transaction ${tx.id()} removed due to ${extractErrorMessage(error)}")
                      traceLogger.trace(error.toString)
                      this.removeFromBothPools(tx.id())
                      r.copy(
                        iterations = r.iterations + 1,
                        validatedTransactions = r.validatedTransactions + tx.id(),
                        checkedAddresses = newCheckedAddresses
                      )
                  }
                }
              }
          }

      @tailrec
      def pack(seed: PackResult): PackResult = {
        if (isTimeLimitReached && seed.transactions.exists(_.nonEmpty) || (transactions.isEmpty && priorityTransactions.isEmpty)) seed
        else {
          val newSeed = packIteration(
            seed.copy(checkedAddresses = Set.empty),
            this.createTxEntrySeq().iterator
          )
          if (newSeed.constraint.isFull) {
            log.trace(s"Block is full: ${newSeed.constraint}")
            newSeed
          } else if (transactions.keys().asScala.forall(newSeed.validatedTransactions)) {
            log.trace("No more transactions to validate")
            newSeed
          } else pack(newSeed)
        }
      }

      pack(PackResult(None, Monoid[Diff].empty, initialConstraint, 0, Set.empty, Set.empty))
    }

    log.trace(
      s"Validated ${packResult.validatedTransactions.size} transactions, " +
        s"of which ${packResult.transactions.fold(0)(_.size)} were packed, ${transactions.size() + priorityTransactions.size} transactions remaining"
    )

    packResult.transactions.map(_.reverse) -> packResult.constraint
  }

  private[this] val traceLogger = LoggerFacade(LoggerFactory.getLogger(this.getClass.getCanonicalName + ".trace"))
  traceLogger.trace("Validation trace reporting is enabled")

  @scala.annotation.tailrec
  private def extractErrorMessage(error: ValidationError): String = error match {
    case see: TxValidationError.ScriptExecutionError        => s"ScriptExecutionError(${see.error})"
    case _: TxValidationError.TransactionNotAllowedByScript => "TransactionNotAllowedByScript"
    case TransactionValidationError(cause, _)               => extractErrorMessage(cause)
    case other                                              => other.toString
  }

  //noinspection ScalaStyle
  private[this] object TxCheck {
    private[this] val ExpirationTime = blockchain.settings.functionalitySettings.maxTransactionTimeBackOffset.toMillis

    def isExpired(transaction: Transaction): Boolean =
      (time.correctedTime() - transaction.timestamp) > ExpirationTime

    def isScripted(transaction: Transaction): Boolean =
      transaction match {
        case _: InvokeScriptTransaction => true
        case _: ExchangeTransaction     => false
        case a: AuthorizedTransaction   => blockchain.hasScript(a.sender.toAddress)
        case _                          => false
      }

    def canCreateAlias(alias: Alias): Boolean =
      blockchain.canCreateAlias(alias)

    def canReissue(asset: IssuedAsset): Boolean =
      blockchain.assetDescription(asset).forall(_.reissuable)
  }

  private[this] object TxCleanup {
    private[this] val scheduled = AtomicBoolean(false)

    def runCleanupAsync(): Unit = if (scheduled.compareAndSet(false, true)) {
      cleanupScheduler.execute { () =>
        try packUnconfirmed(MultiDimensionalMiningConstraint.unlimited, ScalaDuration.Inf)
        finally scheduled.set(false)
      }
    }
  }

  /** DOES NOT verify transactions */
  def addAndCleanup(transactions: Seq[Transaction]): Unit = priorityTransactions.synchronized {
    transactions.foreach { tx =>
      addTransaction(tx, verify = false, this.enablePriorityPool)
      if (this.enablePriorityPool) removeFromOrdPool(tx.id())
    }
    TxCleanup.runCleanupAsync()
  }

  override def close(): Unit = {
    cleanupScheduler.shutdown()
  }

  //noinspection TypeAnnotation
  private[this] object PoolMetrics {
    private[this] val SampleInterval: Duration = Duration.of(500, ChronoUnit.MILLIS)

    private[this] val sizeStats  = Kamon.rangeSampler("utx.pool-size", MeasurementUnit.none, SampleInterval)
    private[this] val bytesStats = Kamon.rangeSampler("utx.pool-bytes", MeasurementUnit.information.bytes, SampleInterval)

    private[this] val prioritySizeStats  = Kamon.rangeSampler("utx.priority-pool-size", MeasurementUnit.none, SampleInterval)
    private[this] val priorityBytesStats = Kamon.rangeSampler("utx.priority-pool-bytes", MeasurementUnit.information.bytes, SampleInterval)

    val putTimeStats    = Kamon.timer("utx.put-if-new")
    val putRequestStats = Kamon.counter("utx.put-if-new.requests")
    val packTimeStats   = Kamon.timer("utx.pack-unconfirmed")

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

    def addTransactionPriority(tx: Transaction): Unit = {
      prioritySizeStats.increment()
      priorityBytesStats.increment(tx.bytes().length)
    }

    def removeTransactionPriority(tx: Transaction): Unit = {
      prioritySizeStats.decrement()
      priorityBytesStats.decrement(tx.bytes().length)
    }
  }
}

object UtxPoolImpl {
  private case class PackResult(
      transactions: Option[Seq[Transaction]],
      totalDiff: Diff,
      constraint: MultiDimensionalMiningConstraint,
      iterations: Int,
      checkedAddresses: Set[Address],
      validatedTransactions: Set[ByteStr]
  )

  private class PessimisticPortfolios(spendableBalanceChanged: Observer[(Address, Asset)], isTxKnown: ByteStr => Boolean) {
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
        if !isTxKnown(txId)
        txPortfolios = transactionPortfolios.getOrDefault(txId, Map.empty[Address, Portfolio])
        txAccountPortfolio <- txPortfolios.get(accountAddr).toSeq
      } yield txAccountPortfolio

      Monoid.combineAll(portfolios)
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
