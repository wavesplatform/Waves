package com.wavesplatform.utx

import java.time.Duration
import java.time.temporal.ChronoUnit
import java.util.concurrent.ConcurrentHashMap

import cats.Monoid
import cats.syntax.monoid._
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.TransactionsOrdering
import com.wavesplatform.events.UtxEvent
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics._
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.settings.UtxSettings
import com.wavesplatform.state.InvokeScriptResult.ErrorMessage
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
import com.wavesplatform.utx.UtxPool.PackStrategy
import kamon.Kamon
import kamon.metric.MeasurementUnit
import monix.execution.ExecutionModel
import monix.execution.atomic.AtomicBoolean
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observer
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import scala.util.{Left, Right}

//noinspection ScalaStyle
class UtxPoolImpl(
    time: Time,
    blockchain: Blockchain,
    spendableBalanceChanged: Observer[(Address, Asset)],
    utxSettings: UtxSettings,
    onEvent: UtxEvent => Unit = _ => (),
    nanoTimeSource: () => TxTimestamp = () => System.nanoTime()
) extends ScorexLogging
    with AutoCloseable
    with UtxPool {

  import com.wavesplatform.utx.UtxPoolImpl._

  val priorityPool = new UtxPriorityPool(blockchain)

  // Context
  private[this] val cleanupScheduler: SchedulerService =
    Schedulers.singleThread("utx-pool-cleanup", executionModel = ExecutionModel.AlwaysAsyncExecution)

  // State
  private[this] val transactions          = new ConcurrentHashMap[ByteStr, Transaction]()
  private[this] val pessimisticPortfolios = new PessimisticPortfolios(spendableBalanceChanged, blockchain.transactionMeta(_).isDefined) // TODO delete in the future

  private[this] val inUTXPoolOrdering = TransactionsOrdering.InUTXPool(utxSettings.fastLaneAddresses)

  override def putIfNew(tx: Transaction, forceValidate: Boolean): TracedResult[ValidationError, Boolean] = {
    if (transactions.containsKey(tx.id()) || priorityPool.contains(tx.id())) TracedResult.wrapValue(false)
    else putNewTx(tx, verify = true, forceValidate)
  }

  private[utx] def putNewTx(tx: Transaction, verify: Boolean, forceValidate: Boolean): TracedResult[ValidationError, Boolean] = {
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
          if (utxSettings.blacklistSenderAddresses.isEmpty || checkWhitelisted(tx)) {
            Right(())
          } else {
            val sender: Option[String] = tx match {
              case x: Authorized => Some(x.sender.toAddress.toString)
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

        def checkWhitelisted(tx: Transaction): Boolean = PoolMetrics.checkWhitelisted.measure {
          inUTXPoolOrdering.isWhitelisted(tx)
        }

        def checkIsMostProfitable(newTx: Transaction): Boolean = PoolMetrics.checkIsMostProfitable.measure {
          transactions
            .values()
            .asScala
            .forall(poolTx => inUTXPoolOrdering.compare(newTx, poolTx) < 0)
        }
      }

      lazy val skipSizeCheck = LimitChecks.checkWhitelisted(tx) || (utxSettings.allowSkipChecks && LimitChecks.checkIsMostProfitable(tx))
      lazy val transactionsBytes = transactions.values.asScala // Bytes size of all transactions in pool
        .map(_.bytes().length)
        .sum

      for {
        _ <- Either.cond(skipSizeCheck || transactions.size < utxSettings.maxSize, (), GenericError("Transaction pool size limit is reached"))
        _ <- Either.cond(
          skipSizeCheck || (transactionsBytes + tx.bytesSize) <= utxSettings.maxBytesSize,
          (),
          GenericError("Transaction pool bytes size limit is reached")
        )

        _ <- LimitChecks.checkNotBlacklisted(tx)
        _ <- LimitChecks.checkScripted(tx, skipSizeCheck)
        _ <- LimitChecks.checkAlias(tx)
        _ <- LimitChecks.canReissue(tx)
      } yield ()
    } else Right(())

    val tracedIsNew = TracedResult(checks).flatMap(_ => addTransaction(tx, verify, forceValidate))
    tracedIsNew.resultE match {
      case Right(isNew) => log.trace(s"putIfNew(${tx.id()}) succeeded, isNew = $isNew")
      case Left(err) =>
        log.debug(s"putIfNew(${tx.id()}) failed with ${extractErrorMessage(err)}")
        traceLogger.trace(err.toString)
    }
    tracedIsNew
  }

  override def removeAll(txs: Iterable[Transaction]): Unit = {
    if (txs.isEmpty) return
    val ids = txs.map(_.id()).toSet
    removeIds(ids)
  }

  def setPriorityDiffs(discDiffs: Seq[Diff]): Unit = {
    val txs = priorityPool.setPriorityDiffs(discDiffs)
    txs.foreach(addTransaction(_, verify = false, canLock = false))
  }

  def resetPriorityPool(): Unit = {
    val txs = priorityPool.clear()
    txs.foreach(addTransaction(_, verify = false))
  }

  private[this] def removeFromOrdPool(txId: ByteStr): Option[Transaction] = {
    for (tx <- Option(transactions.remove(txId))) yield {
      PoolMetrics.removeTransaction(tx)
      pessimisticPortfolios.remove(txId)
      tx
    }
  }

  private[this] def removeIds(removed: Set[ByteStr]): Unit = {
    val priorityRemoved = priorityPool.removeIds(removed)
    val factRemoved     = priorityRemoved ++ removed.flatMap(id => removeFromOrdPool(id))
    factRemoved.foreach(tx => onEvent(UtxEvent.TxRemoved(tx, None)))
  }

  private[utx] def addTransaction(
      tx: Transaction,
      verify: Boolean,
      forceValidate: Boolean = false,
      canLock: Boolean = true
  ): TracedResult[ValidationError, Boolean] = {
    val diffEi = {
      def calculateDiff() = {
        if (forceValidate)
          TransactionDiffer.forceValidate(blockchain.lastBlockTimestamp, time.correctedTime())(priorityPool.compositeBlockchain, tx)
        else
          TransactionDiffer.limitedExecution(blockchain.lastBlockTimestamp, time.correctedTime(), verify)(priorityPool.compositeBlockchain, tx)
      }

      if (canLock) priorityPool.optimisticRead(calculateDiff())(_.resultE.isLeft)
      else calculateDiff()
    }

    def addPortfolio(): Unit = diffEi.map { diff =>
      pessimisticPortfolios.add(tx.id(), diff)
      onEvent(UtxEvent.TxAdded(tx, diff))
    }

    if (!verify || diffEi.resultE.isRight) {
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

  override def pessimisticPortfolio(addr: Address): Portfolio = {
    val priority    = priorityPool.pessimisticPortfolios(addr)
    val pessimistic = pessimisticPortfolios.getAggregated(addr)
    Monoid.combineAll(priority :+ pessimistic)
  }

  private[utx] def nonPriorityTransactions: Seq[Transaction] = {
    transactions.values.asScala.toVector
      .sorted(inUTXPoolOrdering)
  }

  override def all: Seq[Transaction] =
    (priorityPool.priorityTransactions ++ nonPriorityTransactions).distinct

  override def size: Int = transactions.size

  override def transactionById(transactionId: ByteStr): Option[Transaction] =
    Option(transactions.get(transactionId))
      .orElse(priorityPool.transactionById(transactionId))

  private def scriptedAddresses(tx: Transaction): Set[Address] = tx match {
    case t if inUTXPoolOrdering.isWhitelisted(t) => Set.empty
    case i: InvokeScriptTransaction =>
      Set(i.sender.toAddress)
        .filter(blockchain.hasAccountScript) ++ blockchain.resolveAlias(i.dAppAddressOrAlias).fold[Set[Address]](_ => Set.empty, Set(_))
    case e: ExchangeTransaction =>
      Set(e.sender.toAddress, e.buyOrder.sender.toAddress, e.sellOrder.sender.toAddress).filter(blockchain.hasAccountScript)
    case a: Authorized if blockchain.hasAccountScript(a.sender.toAddress) => Set(a.sender.toAddress)
    case _                                                                => Set.empty
  }

  private[this] case class TxEntry(tx: Transaction, priority: Boolean)

  private[this] def createTxEntrySeq(): Seq[TxEntry] =
    priorityPool.priorityTransactions.map(TxEntry(_, priority = true)) ++ nonPriorityTransactions.map(
      TxEntry(_, priority = false)
    )

  override def packUnconfirmed(
      initialConstraint: MultiDimensionalMiningConstraint,
      strategy: PackStrategy,
      cancelled: () => Boolean
  ): (Option[Seq[Transaction]], MultiDimensionalMiningConstraint) = {
    pack(TransactionDiffer(blockchain.lastBlockTimestamp, time.correctedTime()))(initialConstraint, strategy, cancelled)
  }

  private def cleanUnconfirmed(): Unit = {
    log.trace(s"Starting UTX cleanup at height ${blockchain.height}")

    pack(TransactionDiffer.limitedExecution(blockchain.lastBlockTimestamp, time.correctedTime()))(
      MultiDimensionalMiningConstraint.unlimited,
      PackStrategy.Unlimited,
      () => false
    )
  }

  private def pack(differ: (Blockchain, Transaction) => TracedResult[ValidationError, Diff])(
      initialConstraint: MultiDimensionalMiningConstraint,
      strategy: PackStrategy,
      cancelled: () => Boolean
  ): (Option[Seq[Transaction]], MultiDimensionalMiningConstraint) = {
    val packResult = PoolMetrics.packTimeStats.measure {
      val startTime = nanoTimeSource()

      def isTimeLimitReached: Boolean = strategy match {
        case PackStrategy.Limit(time)    => (nanoTimeSource() - startTime) >= time.toNanos
        case PackStrategy.Estimate(time) => (nanoTimeSource() - startTime) >= time.toNanos
        case PackStrategy.Unlimited      => false
      }

      def isTimeEstimateReached: Boolean = strategy match {
        case PackStrategy.Estimate(time) => (nanoTimeSource() - startTime) >= time.toNanos
        case _                           => true
      }

      def isUnlimited: Boolean = strategy == PackStrategy.Unlimited

      def packIteration(prevResult: PackResult, sortedTransactions: Iterator[TxEntry]): PackResult =
        sortedTransactions
          .filterNot(e => prevResult.validatedTransactions(e.tx.id()))
          .foldLeft[PackResult](prevResult) {
            case (r, TxEntry(tx, priority)) =>
              def isLimitReached   = r.transactions.exists(_.nonEmpty) && isTimeLimitReached
              def isAlreadyRemoved = !priority && !transactions.containsKey(tx.id())

              if (r.constraint.isFull || isLimitReached || isAlreadyRemoved || cancelled())
                r // don't run any checks here to speed up mining
              else if (TxCheck.isExpired(tx)) {
                log.debug(s"Transaction ${tx.id()} expired")
                this.removeFromOrdPool(tx.id())
                onEvent(UtxEvent.TxRemoved(tx, Some(GenericError("Expired"))))
                r.copy(iterations = r.iterations + 1, removedTransactions = r.removedTransactions + tx.id())
              } else {
                val newScriptedAddresses = scriptedAddresses(tx)
                if (!priority && r.checkedAddresses.intersect(newScriptedAddresses).nonEmpty) r
                else {
                  val updatedBlockchain   = CompositeBlockchain(blockchain, r.totalDiff)
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
                        newDiff.errorMessage(tx.id()) match {
                          case Some(ErrorMessage(code, text)) =>
                            log.trace(s"Packing transaction ${tx.id()} as failed due to $code: $text")

                          case None =>
                            log.trace(s"Packing transaction ${tx.id()}")
                        }

                        PackResult(
                          Some(r.transactions.fold(Seq(tx))(tx +: _)),
                          r.totalDiff.combine(newDiff),
                          updatedConstraint,
                          r.iterations + 1,
                          newCheckedAddresses,
                          r.validatedTransactions + tx.id(),
                          r.removedTransactions
                        )
                      }

                    case Left(TransactionValidationError(AlreadyInTheState(txId, _), tx)) if r.validatedTransactions.contains(tx.id()) =>
                      log.trace(s"Transaction $txId already validated in priority pool")
                      removeFromOrdPool(tx.id())
                      r

                    case Left(error) =>
                      log.debug(s"Transaction ${tx.id()} removed due to ${extractErrorMessage(error)}")
                      traceLogger.trace(error.toString)
                      this.removeFromOrdPool(tx.id())
                      onEvent(UtxEvent.TxRemoved(tx, Some(error)))
                      r.copy(
                        iterations = r.iterations + 1,
                        validatedTransactions = r.validatedTransactions + tx.id(),
                        checkedAddresses = newCheckedAddresses,
                        removedTransactions = r.removedTransactions + tx.id()
                      )
                  }
                }
              }
          }

      @tailrec
      def loop(seed: PackResult): PackResult = {
        def allValidated(seed: PackResult): Boolean =
          (transactions.keys().asScala ++ priorityPool.priorityTransactionIds).forall(seed.validatedTransactions)

        val newSeed = packIteration(
          seed.copy(checkedAddresses = Set.empty),
          this.createTxEntrySeq().iterator
        )
        if (newSeed.constraint.isFull) {
          log.trace(s"Block is full: ${newSeed.constraint}")
          newSeed
        } else {
          if (isTimeEstimateReached && allValidated(newSeed)) {
            log.trace("No more transactions to validate")
            newSeed
          } else {
            val continue = try {
              while (!cancelled() && !isTimeEstimateReached && allValidated(newSeed)) Thread.sleep(200)
              !cancelled() && (!isTimeEstimateReached || isUnlimited)
            } catch {
              case _: InterruptedException =>
                false
            }
            if (continue) loop(newSeed)
            else newSeed
          }
        }
      }

      loop(PackResult(None, Monoid[Diff].empty, initialConstraint, 0, Set.empty, Set.empty, Set.empty))
    }

    log.trace(
      s"Validated ${packResult.validatedTransactions.size} transactions, " +
        s"of which ${packResult.transactions.fold(0)(_.size)} were packed, ${transactions.size() + priorityPool.priorityTransactions.size} transactions remaining"
    )

    if (packResult.removedTransactions.nonEmpty) log.trace(s"Removing invalid transactions: ${packResult.removedTransactions.mkString(", ")}")
    priorityPool.invalidateTxs(packResult.removedTransactions)
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
        case a: AuthorizedTransaction   => blockchain.hasAccountScript(a.sender.toAddress)
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
      cleanupLoop()
    }

    private def cleanupLoop(): Unit = cleanupScheduler.execute { () =>
      while (scheduled.compareAndSet(true, false)) {
        if (!transactions.isEmpty || priorityPool.priorityTransactions.nonEmpty) {
          cleanUnconfirmed()
        }
      }
    }
  }

  /** DOES NOT verify transactions */
  def addAndCleanup(transactions: Iterable[Transaction]): Unit = {
    transactions.foreach(addTransaction(_, verify = false))
    TxCleanup.runCleanupAsync()
  }

  def runCleanup(): Unit = {
    TxCleanup.runCleanupAsync()
  }

  override def close(): Unit = {
    import scala.concurrent.duration._
    cleanupScheduler.shutdown()
    cleanupScheduler.awaitTermination(10 seconds)
  }

  override def finalize(): Unit = {
    cleanupScheduler.shutdown()
  }

  //noinspection TypeAnnotation
  private[this] object PoolMetrics {
    private[this] val SampleInterval: Duration = Duration.of(500, ChronoUnit.MILLIS)

    private[this] val sizeStats  = Kamon.rangeSampler("utx.pool-size", MeasurementUnit.none, SampleInterval).withoutTags()
    private[this] val bytesStats = Kamon.rangeSampler("utx.pool-bytes", MeasurementUnit.information.bytes, SampleInterval).withoutTags()

    val putTimeStats    = Kamon.timer("utx.put-if-new").withoutTags()
    val putRequestStats = Kamon.counter("utx.put-if-new.requests").withoutTags()
    val packTimeStats   = Kamon.timer("utx.pack-unconfirmed").withoutTags()

    val checkIsMostProfitable = Kamon.timer("utx.check.is-most-profitable").withoutTags()
    val checkAlias            = Kamon.timer("utx.check.alias").withoutTags()
    val checkCanReissue       = Kamon.timer("utx.check.can-reissue").withoutTags()
    val checkNotBlacklisted   = Kamon.timer("utx.check.not-blacklisted").withoutTags()
    val checkScripted         = Kamon.timer("utx.check.scripted").withoutTags()
    val checkWhitelisted      = Kamon.timer("utx.check.whitelisted").withoutTags()

    def addTransaction(tx: Transaction): Unit = {
      sizeStats.increment()
      bytesStats.increment(tx.bytesSize)
    }

    def removeTransaction(tx: Transaction): Unit = {
      sizeStats.decrement()
      bytesStats.decrement(tx.bytesSize)
    }
  }
}

private object UtxPoolImpl {
  case class PackResult(
      transactions: Option[Seq[Transaction]],
      totalDiff: Diff,
      constraint: MultiDimensionalMiningConstraint,
      iterations: Int,
      checkedAddresses: Set[Address],
      validatedTransactions: Set[ByteStr],
      removedTransactions: Set[ByteStr]
  )

  class PessimisticPortfolios(spendableBalanceChanged: Observer[(Address, Asset)], isTxKnown: ByteStr => Boolean) {
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
