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
import monix.execution.{AsyncQueue, CancelableFuture, Scheduler}
import monix.reactive.Observable

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

trait UtxPoolSynchronizer {
  def tryPublish(tx: Transaction, source: Channel): Unit
  def publish(tx: Transaction): TracedResult[ValidationError, Boolean]
}

class UtxPoolSynchronizerImpl(
    val settings: UtxSynchronizerSettings,
    putIfNew: Transaction => TracedResult[ValidationError, Boolean],
    broadcast: (Transaction, Option[Channel]) => Unit,
    blockSource: Observable[LastBlockInfo]
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
  cancelableFuture.onComplete {
    case Success(_) => log.info("UtxPoolSynchronizer stopped")
    case Failure(e) => log.warn("UtxPoolSynchronizer stopped abnormally", e)
  }

  blockSource.map(_.height).distinctUntilChanged.foreach(_ => knownTransactions.invalidateAll())

  private def transactionIsNew(txId: ByteStr): Boolean = {
    var isNew = false
    knownTransactions.get(txId, { () =>
      isNew = true; dummy
    })
    isNew
  }

  override def tryPublish(tx: Transaction, source: Channel): Unit =
    if (!transactionIsNew(tx.id())) {
      log.trace(s"Ignoring known transaction ${tx.id()}")
    } else if (queue.tryOffer(tx -> source)) {
      log.trace(s"Queueing transaction ${tx.id()} for processing")
    } else {
      log.trace(s"Dropping transaction ${tx.id()} because queue is full (isEmpty=${queue.isEmpty})")
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
    Await.result(validateFuture(tx, settings.allowTxRebroadcasting, None), 10.seconds)

  override def close(): Unit = cancelableFuture.cancel()

  private def pollTransactions(): CancelableFuture[Unit] = queue.poll().transformWith { result =>
    result match {
      case Success((tx, source)) =>
        log.trace(s"Consuming transaction ${tx.id()} from $source")
        validateFuture(tx, allowRebroadcast = false, Some(source))
      case Failure(e) =>
        log.warn(s"Error polling transaction queue", e)
    }
    pollTransactions()
  }
}

object UtxPoolSynchronizer extends ScorexLogging {
  def apply(utx: UtxPool, settings: UtxSynchronizerSettings, allChannels: ChannelGroup, blockSource: Observable[LastBlockInfo])(
      implicit sc: Scheduler
  ): UtxPoolSynchronizer = new UtxPoolSynchronizerImpl(settings, tx => utx.putIfNew(tx), (tx, ch) => allChannels.broadcast(tx, ch), blockSource)

}
