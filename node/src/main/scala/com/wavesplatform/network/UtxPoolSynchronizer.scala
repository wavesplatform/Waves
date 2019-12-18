package com.wavesplatform.network

import cats.instances.int._
import com.google.common.cache.CacheBuilder
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.SynchronizationSettings.UtxSynchronizerSettings
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{LastBlockInfo, Transaction}
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.utx.UtxPool
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.execution.Ack.Continue
import monix.execution.{Ack, AsyncQueue, CancelableFuture, Scheduler}
import monix.reactive.Observable
import monix.reactive.Observable.Operator
import monix.reactive.observers.Subscriber

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Success

trait UtxPoolSynchronizer {
  def tryPublish(tx: Transaction, source: Channel): Unit
  def publish(tx: Transaction): TracedResult[ValidationError, Boolean]
}

class UtxPoolSynchronizerImpl(
    val settings: UtxSynchronizerSettings,
    putIfNew: Transaction => TracedResult[ValidationError, Boolean],
    broadcast: (Transaction, Option[Channel]) => Unit,
    blockSource: Observable[LastBlockInfo],
    readiness: Observable[Boolean]
)(
    implicit val scheduler: Scheduler
) extends UtxPoolSynchronizer
    with ScorexLogging
    with AutoCloseable {

  val queue = AsyncQueue.bounded[(Transaction, Channel)](settings.maxQueueSize)

  val dummy = new Object()
  val knownTransactions = CacheBuilder
    .newBuilder()
    .maximumSize(settings.networkTxCacheSize)
    .build[ByteStr, Object]

  val cancelableFuture = pollTransactions()

  blockSource.map(_.height).distinctUntilChanged.foreach(_ => knownTransactions.invalidateAll())

  private val lastReadiness = lastObserved(readiness.liftByOperator(new UtxPoolSynchronizer.UtxReadinessOperator)).map(_.contains(true))

  private def transactionIsNew(txId: ByteStr): Boolean = {
    var isNew = false
    knownTransactions.get(txId, { () =>
      isNew = true; dummy
    })
    isNew
  }

  override def tryPublish(tx: Transaction, source: Channel): Unit = if (transactionIsNew(tx.id())) {
    queue.tryOffer(tx -> source)
  }

  private def validateFuture(tx: Transaction, allowRebroadcast: Boolean, source: Option[Channel]): Future[TracedResult[ValidationError, Boolean]] =
    Future(putIfNew(tx))(scheduler)
      .recover {
        case t =>
          log.warn(s"Error validating transaction ${tx.id()}", t)
          TracedResult(Left(GenericError(t)))
      }
      .andThen {
        case Success(TracedResult(Right(isNew), _)) if isNew || allowRebroadcast => broadcast(tx, source)
      }

  override def publish(tx: Transaction): TracedResult[ValidationError, Boolean] =
    if (lastReadiness())
      Await.result(validateFuture(tx, settings.allowTxRebroadcasting, None), Duration.Inf)
    else {
      log.trace(s"Blockchain is not ready yet, discarding transaction: ${tx.id()}")
      Right(false)
    }

  override def close(): Unit = cancelableFuture.cancel()

  private def pollTransactions(): CancelableFuture[Unit] = queue.poll().flatMap {
    case (tx, source) =>
      validateFuture(tx, allowRebroadcast = false, Some(source))
      pollTransactions()
  }
}

object UtxPoolSynchronizer extends ScorexLogging {
  def apply(
      utx: UtxPool,
      settings: UtxSynchronizerSettings,
      allChannels: ChannelGroup,
      blockSource: Observable[LastBlockInfo],
      readiness: Observable[Boolean]
  )(
      implicit sc: Scheduler
  ): UtxPoolSynchronizer =
    new UtxPoolSynchronizerImpl(settings, tx => utx.putIfNew(tx), (tx, ch) => allChannels.broadcast(tx, ch), blockSource, readiness)

  /** Memoized readiness operator */
  class UtxReadinessOperator extends Operator[Boolean, Boolean] {
    private[this] var initialized = false
    override def apply(out: Subscriber[Boolean]): Subscriber[Boolean] =
      new Subscriber[Boolean] {
        override def onNext(elem: Boolean): Future[Ack] =
          if (initialized) Continue
          else {
            initialized = elem
            out.onNext(elem)
          }

        override def onError(ex: Throwable): Unit  = out.onError(ex)
        override def onComplete(): Unit            = out.onComplete()
        override implicit def scheduler: Scheduler = out.scheduler
      }
  }
}
