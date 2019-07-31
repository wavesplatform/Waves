package com.wavesplatform.network

import com.google.common.cache.CacheBuilder
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.UtxPoolSynchronizer.TxAddResult
import com.wavesplatform.settings.SynchronizationSettings.UtxSynchronizerSettings
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.utx.UtxPool
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.eval.Task
import monix.execution.{AsyncQueue, Cancelable, CancelableFuture, Scheduler}
import monix.reactive.subjects.ConcurrentSubject
import monix.reactive.{Consumer, Observable}

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

trait UtxPoolSynchronizer {
  def settings: UtxSynchronizerSettings
  def publishTransaction(tx: Transaction, source: Channel = null, forceBroadcast: Boolean = false): Future[TxAddResult]
}

class UtxPoolSynchronizerImpl(utx: UtxPool, val settings: UtxSynchronizerSettings, allChannels: ChannelGroup, blockSource: Observable[_])(
    implicit val scheduler: Scheduler)
    extends UtxPoolSynchronizer
    with ScorexLogging
    with AutoCloseable {

  private[this] case class BroadcastRequest(transaction: Transaction, source: Channel, promise: Promise[TxAddResult], forceBroadcast: Boolean)
  private[this] lazy val txSource = ConcurrentSubject.publishToOne[BroadcastRequest]

  private[this] val future = start(txSource, blockSource)

  //noinspection ScalaStyle
  override def publishTransaction(tx: Transaction, source: Channel = null, forceBroadcast: Boolean = false): Future[TxAddResult] = {
    val promise = Promise[TxAddResult]
    txSource.onNext(BroadcastRequest(tx, source, promise, forceBroadcast))
    promise.future
  }

  override def close(): Unit =
    future.cancel()

  private[this] def putAndBroadcastTask(source: Observable[BroadcastRequest]): Task[Unit] = {
    val queue = AsyncQueue.bounded[BroadcastRequest](settings.maxQueueSize)

    val produceTask = source.observeOn(scheduler).foreachL { req =>
      if (!queue.tryOffer(req))
        req.promise.tryFailure(new RuntimeException("UTX queue overflow"))
    }

    val consumeTask = Observable
      .repeatEvalF(Task.deferFuture(queue.drain(1, (settings.maxQueueSize / 10) max 1)))
      .observeOn(scheduler)
      .flatMap(Observable.fromIterable)
      .mapParallelUnordered(settings.parallelism) {
        case BroadcastRequest(transaction, sender, promise, forceBroadcast) =>
          Task {
            val value = concurrent.blocking(utx.putIfNew(transaction))
            promise.success(value)

            value.resultE.toOption.filter(_ || forceBroadcast).map { _ =>
              log.trace(s"Broadcasting ${transaction.id()} to ${allChannels.size()} peers except $sender")
              allChannels.write(transaction, (_: Channel) != sender)
            }
          }.onErrorHandle { error =>
            promise.tryFailure(error)
            None
          }
      }
      .bufferTimedAndCounted(settings.maxBufferTime, settings.maxBufferSize)
      .filter(_.flatten.nonEmpty)
      .foreachL(_ => allChannels.flush())

    Task.parMap2(produceTask, consumeTask)((_, _) => ())
  }

  private[this] def start(txSource: Observable[BroadcastRequest], blockSource: Observable[_]): CancelableFuture[Unit] = {
    val dummy = new Object()
    val knownTransactions = CacheBuilder
      .newBuilder()
      .maximumSize(settings.networkTxCacheSize)
      .build[ByteStr, Object]

    val blockCacheCleaning = blockSource
      .observeOn(scheduler)
      .foreachL(_ => knownTransactions.invalidateAll())
      .runAsyncLogErr

    val newTxSource = txSource
      .observeOn(scheduler)
      .filter {
        case BroadcastRequest(tx, _, _, forceBroadcast) =>
          var isNew = false
          knownTransactions.get(tx.id(), { () =>
            isNew = true; dummy
          })
          isNew || forceBroadcast
      }

    val synchronizerFuture = putAndBroadcastTask(newTxSource).runAsyncLogErr

    synchronizerFuture.onComplete {
      case Success(_)     => log.info("UtxPoolSynschronizer stops")
      case Failure(error) => log.error("Error in utx pool synchronizer", error)
    }

    synchronizerFuture.onComplete(_ => blockCacheCleaning.cancel())

    CancelableFuture(synchronizerFuture, { () =>
      synchronizerFuture.cancel()
      blockCacheCleaning.cancel()
    })
  }
}

object UtxPoolSynchronizer {
  type TxAddResult = TracedResult[ValidationError, Boolean]

  def apply(utx: UtxPool, settings: UtxSynchronizerSettings, allChannels: ChannelGroup, blockSource: Observable[_])(
      implicit sc: Scheduler): UtxPoolSynchronizer = new UtxPoolSynchronizerImpl(utx, settings, allChannels, blockSource)

  implicit class UtxPoolSynchronizerExt(ups: UtxPoolSynchronizer)(implicit sc: Scheduler) {
    def publishTransactions(obs: ChannelObservable[Transaction]): Cancelable =
      obs
        .consumeWith(
          Consumer.foreachParallelTask(ups.settings.parallelism) { case (c, t) => Task.fromFuture(ups.publishTransaction(t, c)).map(_ => ()) })
        .runAsyncLogErr
  }
}
