package com.wavesplatform.utx

import cats.implicits.catsSyntaxSemigroup
import com.wavesplatform.ResponsivenessLogs
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.TransactionsOrdering
import com.wavesplatform.events.UtxEvent
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.*
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.settings.UtxSettings
import com.wavesplatform.state.InvokeScriptResult.ErrorMessage
import com.wavesplatform.state.TxStateSnapshotHashBuilder.TxStatusInfo
import com.wavesplatform.state.diffs.BlockDiffer.CurrentBlockFeePart
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.state.diffs.{BlockDiffer, TransactionDiffer}
import com.wavesplatform.state.SnapshotBlockchain
import com.wavesplatform.state.{Blockchain, Portfolio, StateSnapshot, TxStateSnapshotHashBuilder}
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.TxValidationError.{AlreadyInTheState, GenericError, SenderIsBlacklisted, WithLog}
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.utils.{LoggerFacade, Schedulers, ScorexLogging, Time}
import com.wavesplatform.utx.UtxPool.PackStrategy
import kamon.Kamon
import kamon.metric.MeasurementUnit
import monix.execution.ExecutionModel
import monix.execution.atomic.AtomicBoolean
import monix.execution.schedulers.SchedulerService
import org.slf4j.LoggerFactory

import java.time.Duration
import java.time.temporal.ChronoUnit
import java.util.concurrent.ConcurrentHashMap
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

//noinspection ScalaStyle
case class UtxPoolImpl(
    time: Time,
    blockchain: Blockchain,
    utxSettings: UtxSettings,
    maxTxErrorLogSize: Int,
    isMiningEnabled: Boolean,
    onEvent: UtxEvent => Unit = _ => (),
    nanoTimeSource: () => TxTimestamp = () => System.nanoTime()
) extends ScorexLogging
    with AutoCloseable
    with UtxPool {

  import com.wavesplatform.utx.UtxPoolImpl.*

  // Context
  private[this] val cleanupScheduler: SchedulerService =
    Schedulers.singleThread("utx-pool-cleanup", executionModel = ExecutionModel.AlwaysAsyncExecution)
  private[this] val inUTXPoolOrdering = TransactionsOrdering.InUTXPool(utxSettings.fastLaneAddresses)

  // State
  val priorityPool               = new UtxPriorityPool(blockchain)
  private[this] val transactions = new ConcurrentHashMap[ByteStr, Transaction]()

  override def getPriorityPool: Option[UtxPriorityPool] = Some(priorityPool)

  override def putIfNew(tx: Transaction, forceValidate: Boolean): TracedResult[ValidationError, Boolean] = {
    if (transactions.containsKey(tx.id()) || priorityPool.contains(tx.id())) TracedResult.wrapValue(false)
    else putNewTx(tx, forceValidate)
  }

  private[utx] def putNewTx(tx: Transaction, forceValidate: Boolean): TracedResult[ValidationError, Boolean] = {
    PoolMetrics.putRequestStats.increment()

    val checks = PoolMetrics.putTimeStats.measure {
      object LimitChecks {
        def checkScripted(tx: Transaction, skipSizeCheck: () => Boolean): Either[GenericError, Transaction] =
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
                  skipSizeCheck() || transactions.values().asScala.count(TxCheck.isScripted) < utxSettings.maxScriptedSize,
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
                    recipients.forall(r => utxSettings.allowBlacklistedTransferTo.contains(r.toString))
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
        _ <- Either.cond(
          skipSizeCheck || transactions.size < utxSettings.maxSize,
          (),
          GenericError("Transaction pool size limit is reached")
        )
        _ <- Either.cond(
          skipSizeCheck || (transactionsBytes + tx.bytesSize) <= utxSettings.maxBytesSize,
          (),
          GenericError("Transaction pool bytes size limit is reached")
        )
        _ <- LimitChecks.checkNotBlacklisted(tx)
        _ <- LimitChecks.checkScripted(tx, () => skipSizeCheck)
      } yield ()
    }

    val tracedIsNew = TracedResult(checks).flatMap(_ => addTransaction(tx, verify = true, forceValidate))
    tracedIsNew.resultE match {
      case Right(isNew) =>
        log.trace(s"putIfNew(${tx.id()}) succeeded, isNew = $isNew")
      case Left(err) =>
        log.debug(s"putIfNew(${tx.id()}) failed with ${extractErrorMessage(err)}")
        traceLogger.trace(err match {
          case w: WithLog => w.toStringWithLog(maxTxErrorLogSize)
          case err        => err.toString
        })
    }
    tracedIsNew
  }

  override def removeAll(txs: Iterable[Transaction]): Unit = {
    if (txs.isEmpty) return
    val ids = txs.map(_.id()).toSet
    removeIds(ids)
  }

  def setPrioritySnapshots(discSnapshots: Seq[StateSnapshot]): Unit = {
    val txs = priorityPool.setPriorityDiffs(discSnapshots)
    txs.foreach(addTransaction(_, verify = false, canLock = false))
  }

  def resetPriorityPool(): Unit =
    priorityPool.setPriorityDiffs(Seq.empty)

  private[this] def removeFromOrdPool(txId: ByteStr): Option[Transaction] = {
    for (tx <- Option(transactions.remove(txId))) yield {
      PoolMetrics.removeTransaction(tx)
      tx
    }
  }

  private[this] def removeIds(removed: Set[ByteStr]): Unit = {
    val priorityRemoved = priorityPool.removeIds(removed)
    val factRemoved     = priorityRemoved ++ removed.flatMap(id => removeFromOrdPool(id))
    factRemoved.foreach(TxStateActions.removeMined(_))
  }

  private[utx] def addTransaction(
      tx: Transaction,
      verify: Boolean,
      forceValidate: Boolean = false,
      canLock: Boolean = true
  ): TracedResult[ValidationError, Boolean] = {
    val diffEi = {
      def calculateSnapshot(): TracedResult[ValidationError, StateSnapshot] = {
        if (forceValidate)
          TransactionDiffer.forceValidate(blockchain.lastBlockTimestamp, time.correctedTime(), enableExecutionLog = true)(
            priorityPool.compositeBlockchain,
            tx
          )
        else
          TransactionDiffer.limitedExecution(
            blockchain.lastBlockTimestamp,
            time.correctedTime(),
            utxSettings.alwaysUnlimitedExecution,
            verify,
            enableExecutionLog = true
          )(
            priorityPool.compositeBlockchain,
            tx
          )
      }

      if (canLock) priorityPool.optimisticRead(calculateSnapshot())(_.resultE.isLeft)
      else calculateSnapshot()
    }

    if (!verify || diffEi.resultE.isRight) {
      TxStateActions.addReceived(tx, diffEi.resultE.toOption)
    }

    diffEi.map(_ => true)
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
      Set(i.senderAddress).filter(blockchain.hasAccountScript) ++ blockchain.resolveAlias(i.dApp).toOption
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
      prevStateHash: Option[ByteStr],
      strategy: PackStrategy,
      cancelled: () => Boolean
  ): (Option[Seq[Transaction]], MultiDimensionalMiningConstraint, Option[ByteStr]) = {
    pack(TransactionDiffer(blockchain.lastBlockTimestamp, time.correctedTime(), enableExecutionLog = true))(
      initialConstraint,
      strategy,
      prevStateHash,
      cancelled
    )
  }

  def cleanUnconfirmed(): Unit = {
    log.trace(s"Starting UTX cleanup at height ${blockchain.height}")

    this.transactions
      .values()
      .asScala
      .foreach { tx =>
        if (TxCheck.isExpired(tx)) {
          TxStateActions.removeExpired(tx)
        } else {
          val differ = if (!isMiningEnabled && utxSettings.forceValidateInCleanup) {
            TransactionDiffer.forceValidate(blockchain.lastBlockTimestamp, time.correctedTime(), enableExecutionLog = true)(
              priorityPool.compositeBlockchain,
              _
            )
          } else {
            TransactionDiffer.limitedExecution(
              blockchain.lastBlockTimestamp,
              time.correctedTime(),
              utxSettings.alwaysUnlimitedExecution,
              enableExecutionLog = true
            )(
              priorityPool.compositeBlockchain,
              _
            )
          }
          val diffEi = differ(tx).resultE
          diffEi.left.foreach { error =>
            TxStateActions.removeInvalid("Cleanup", tx, error)
          }
        }
      }
  }

  private def removeInvalid(
      r: PackResult,
      tx: Transaction,
      checkedAddresses: Set[Address],
      error: ValidationError
  ): PackResult = {
    TxStateActions.removeInvalid("Pack", tx, error)
    r.copy(
      iterations = r.iterations + 1,
      validatedTransactions = r.validatedTransactions + tx.id(),
      checkedAddresses = checkedAddresses,
      removedTransactions = r.removedTransactions + tx.id()
    )
  }

  private def pack(differ: (Blockchain, Transaction) => TracedResult[ValidationError, StateSnapshot])(
      initialConstraint: MultiDimensionalMiningConstraint,
      strategy: PackStrategy,
      prevStateHash: Option[ByteStr],
      cancelled: () => Boolean
  ): (Option[Seq[Transaction]], MultiDimensionalMiningConstraint, Option[ByteStr]) = {
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

      def minerFeePortfolio(currBlockchain: Blockchain, tx: Transaction): Map[Address, Portfolio] = {
        val (feeAsset, feeAmount) = BlockDiffer.maybeApplySponsorship(currBlockchain, blockchain.isSponsorshipActive, tx.assetFee)
        val minerPortfolio = if (!blockchain.isNGActive) Portfolio.empty else Portfolio.build(feeAsset, feeAmount).multiply(CurrentBlockFeePart)

        Map(currBlockchain.lastBlockHeader.get.header.generator.toAddress -> minerPortfolio)
      }

      def packIteration(prevResult: PackResult, sortedTransactions: Iterator[TxEntry]): PackResult =
        sortedTransactions
          .filterNot(e => prevResult.validatedTransactions(e.tx.id()))
          .foldLeft[PackResult](prevResult) { case (r, TxEntry(tx, priority)) =>
            def isLimitReached   = r.transactions.exists(_.nonEmpty) && isTimeLimitReached
            def isAlreadyRemoved = !priority && !transactions.containsKey(tx.id())

            if (r.constraint.isFull || isLimitReached || isAlreadyRemoved || cancelled())
              r // don't run any checks here to speed up mining
            else if (TxCheck.isExpired(tx)) {
              TxStateActions.removeExpired(tx)
              r.copy(iterations = r.iterations + 1, removedTransactions = r.removedTransactions + tx.id())
            } else {
              val newScriptedAddresses = scriptedAddresses(tx)
              if (!priority && r.checkedAddresses.intersect(newScriptedAddresses).nonEmpty) r
              else {
                val updatedBlockchain   = SnapshotBlockchain(blockchain, r.totalSnapshot)
                val newCheckedAddresses = newScriptedAddresses ++ r.checkedAddresses
                val e                   = differ(updatedBlockchain, tx).resultE
                e match {
                  case Right(newSnapshot) =>
                    val updatedConstraint = r.constraint.put(updatedBlockchain, tx, newSnapshot)
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
                      newSnapshot.errorMessage(tx.id()) match {
                        case Some(ErrorMessage(code, text)) =>
                          log.trace(s"Packing transaction ${tx.id()} as failed due to $code: $text")

                        case None =>
                          log.trace(s"Packing transaction ${tx.id()}")
                      }

                      (for {
                        resultSnapshot <- (r.totalSnapshot |+| newSnapshot)
                          .addBalances(minerFeePortfolio(updatedBlockchain, tx), updatedBlockchain)
                        fullTxSnapshot <- newSnapshot.addBalances(minerFeePortfolio(updatedBlockchain, tx), updatedBlockchain)
                      } yield {
                        val txInfo = newSnapshot.transactions.head._2
                        PackResult(
                          Some(r.transactions.fold(Seq(tx))(tx +: _)),
                          resultSnapshot,
                          updatedConstraint,
                          r.iterations + 1,
                          newCheckedAddresses,
                          r.validatedTransactions + tx.id(),
                          r.removedTransactions,
                          r.stateHash
                            .map(prevStateHash =>
                              TxStateSnapshotHashBuilder
                                .createHashFromSnapshot(fullTxSnapshot, Some(TxStatusInfo(txInfo.transaction.id(), txInfo.status)))
                                .createHash(prevStateHash)
                            )
                        )
                      }).fold(
                        error => removeInvalid(r, tx, newCheckedAddresses, GenericError(error)),
                        identity
                      )
                    }

                  case Left(TransactionValidationError(AlreadyInTheState(txId, _), tx)) if r.validatedTransactions.contains(tx.id()) =>
                    log.trace(s"Transaction $txId already validated in priority pool")
                    removeFromOrdPool(tx.id()) // Dont run events/metrics publication here because the tx is still exists in the priority pool
                    r

                  case Left(error) =>
                    removeInvalid(r, tx, newCheckedAddresses, error)
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
            val continue =
              try {
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

      loop(
        PackResult(
          None,
          StateSnapshot.empty,
          initialConstraint,
          0,
          Set.empty,
          Set.empty,
          Set.empty,
          prevStateHash
        )
      )
    }

    log.trace(
      s"Validated ${packResult.validatedTransactions.size} transactions, " +
        s"of which ${packResult.transactions.fold(0)(_.size)} were packed, ${transactions.size() + priorityPool.priorityTransactions.size} transactions remaining"
    )

    if (packResult.removedTransactions.nonEmpty) log.trace(s"Removing invalid transactions: ${packResult.removedTransactions.mkString(", ")}")
    priorityPool.invalidateTxs(packResult.removedTransactions)
    (packResult.transactions.map(_.reverse), packResult.constraint, packResult.stateHash)
  }

  private[this] val traceLogger = LoggerFacade(LoggerFactory.getLogger(this.getClass.getCanonicalName + ".trace"))
  traceLogger.trace("Validation trace reporting is enabled")

  @scala.annotation.tailrec
  private def extractErrorClass(error: ValidationError): ValidationError = error match {
    case TransactionValidationError(cause, _) => extractErrorClass(cause)
    case other                                => other
  }

  @scala.annotation.tailrec
  private def extractErrorMessage(error: ValidationError): String = error match {
    case see: TxValidationError.ScriptExecutionError        => s"ScriptExecutionError(${see.message})"
    case _: TxValidationError.TransactionNotAllowedByScript => "TransactionNotAllowedByScript"
    case TransactionValidationError(cause, _)               => extractErrorMessage(cause)
    case other                                              => other.toString
  }

  private[this] object TxStateActions {
    def addReceived(tx: Transaction, snapshot: Option[StateSnapshot]): Unit =
      if (transactions.putIfAbsent(tx.id(), tx) == null) {
        snapshot.foreach(s => onEvent(UtxEvent.TxAdded(tx, s)))
        PoolMetrics.addTransaction(tx)
        ResponsivenessLogs.writeEvent(blockchain.height, tx, ResponsivenessLogs.TxEvent.Received)
      }

    def removeMined(tx: Transaction): Unit = {
      ResponsivenessLogs.writeEvent(blockchain.height, tx, ResponsivenessLogs.TxEvent.Mined)
      onEvent(UtxEvent.TxRemoved(tx, None))
    }

    def removeInvalid(cause: String, tx: Transaction, error: ValidationError): Unit =
      removeFromOrdPool(tx.id()).foreach { tx =>
        log.debug(s"$cause: Transaction ${tx.id()} removed due to ${extractErrorMessage(error)}")
        traceLogger.trace(error.toString)

        ResponsivenessLogs.writeEvent(blockchain.height, tx, ResponsivenessLogs.TxEvent.Invalidated, Some(extractErrorClass(error)))
        onEvent(UtxEvent.TxRemoved(tx, Some(error)))
      }

    def removeExpired(tx: Transaction): Unit = {
      log.debug(s"Transaction ${tx.id()} expired")

      ResponsivenessLogs.writeEvent(blockchain.height, tx, ResponsivenessLogs.TxEvent.Expired)
      onEvent(UtxEvent.TxRemoved(tx, Some(GenericError("Expired"))))

      UtxPoolImpl.this.removeFromOrdPool(tx.id())
    }
  }

  // noinspection ScalaStyle
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
  }

  // noinspection NameBooleanParameters
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
  def addAndScheduleCleanup(transactions: Iterable[Transaction]): Unit = {
    transactions.foreach(addTransaction(_, verify = false))
    TxCleanup.runCleanupAsync()
  }

  override def scheduleCleanup(): Unit = {
    TxCleanup.runCleanupAsync()
  }

  override def close(): Unit = {
    import scala.concurrent.duration.*
    cleanupScheduler.shutdown()
    cleanupScheduler.awaitTermination(10 seconds)
  }

  // noinspection TypeAnnotation
  private[this] object PoolMetrics {
    private[this] val SampleInterval: Duration = Duration.of(500, ChronoUnit.MILLIS)

    private[this] val sizeStats         = Kamon.rangeSampler("utx.pool-size", MeasurementUnit.none, SampleInterval).withoutTags()
    private[this] val neutrinoSizeStats = Kamon.rangeSampler("neutrino.utx-pool-size", MeasurementUnit.none, SampleInterval).withoutTags()
    private[this] val bytesStats        = Kamon.rangeSampler("utx.pool-bytes", MeasurementUnit.information.bytes, SampleInterval).withoutTags()

    val putTimeStats    = Kamon.timer("utx.put-if-new").withoutTags()
    val putRequestStats = Kamon.counter("utx.put-if-new.requests").withoutTags()
    val packTimeStats   = Kamon.timer("utx.pack-unconfirmed").withoutTags()

    val checkIsMostProfitable = Kamon.timer("utx.check.is-most-profitable").withoutTags()
    val checkNotBlacklisted   = Kamon.timer("utx.check.not-blacklisted").withoutTags()
    val checkScripted         = Kamon.timer("utx.check.scripted").withoutTags()
    val checkWhitelisted      = Kamon.timer("utx.check.whitelisted").withoutTags()

    def addTransaction(tx: Transaction): Unit = {
      sizeStats.increment()
      bytesStats.increment(tx.bytesSize)
      if (ResponsivenessLogs.isNeutrino(tx)) neutrinoSizeStats.increment()
    }

    def removeTransaction(tx: Transaction): Unit = {
      sizeStats.decrement()
      bytesStats.decrement(tx.bytesSize)
      if (ResponsivenessLogs.isNeutrino(tx)) neutrinoSizeStats.decrement()
    }
  }
}

private object UtxPoolImpl {
  case class PackResult(
      transactions: Option[Seq[Transaction]],
      totalSnapshot: StateSnapshot,
      constraint: MultiDimensionalMiningConstraint,
      iterations: Int,
      checkedAddresses: Set[Address],
      validatedTransactions: Set[ByteStr],
      removedTransactions: Set[ByteStr],
      stateHash: Option[ByteStr]
  )
}
