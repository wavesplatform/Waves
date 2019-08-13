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
import io.netty.channel.group.{ChannelGroup, ChannelGroupFuture}
import monix.eval.{Coeval, Task}
import monix.execution.{AsyncQueue, CancelableFuture, Scheduler}
import monix.reactive.subjects.ConcurrentSubject
import monix.reactive.{Consumer, Observable, OverflowStrategy}

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

//noinspection ScalaStyle
trait UtxPoolSynchronizer {
  def settings: UtxSynchronizerSettings

  def queueAndPublishTransaction(tx: Transaction, source: Channel = null, forceBroadcast: Boolean = false): (Future[Boolean], Future[TxAddResult])

  def publishTransaction(tx: Transaction, source: Channel = null, forceBroadcast: Boolean = false): Future[TxAddResult] =
    queueAndPublishTransaction(tx, source, forceBroadcast)._2
}

class UtxPoolSynchronizerImpl(utx: UtxPool, val settings: UtxSynchronizerSettings, allChannels: ChannelGroup, blockSource: Observable[_])(
    implicit val scheduler: Scheduler)
    extends UtxPoolSynchronizer
    with ScorexLogging
    with AutoCloseable {

  private[this] case class BroadcastRequest(transaction: Transaction, source: Channel, queuePromise: Promise[Boolean], putPromise: Promise[TxAddResult], forceBroadcast: Boolean)
  private[this] lazy val txSource = ConcurrentSubject.publishToOne[BroadcastRequest](OverflowStrategy.DropNewAndSignal(settings.maxQueueSize * 2, dropped => Coeval {
    log.trace(s"$dropped transactions dropped")
    None
  }))

  private[this] val future = start(txSource, blockSource)

  //noinspection ScalaStyle
  override def queueAndPublishTransaction(tx: Transaction, source: Channel = null, forceBroadcast: Boolean = false): (Future[Boolean], Future[TxAddResult]) = {
    val request = BroadcastRequest(tx, source, Promise[Boolean], Promise[TxAddResult], forceBroadcast)
    txSource.onNext(request)
    (request.queuePromise.future, request.putPromise.future)
  }

  override def close(): Unit = {
    log.trace("Stopping UTX pool synchronizer")
    future.cancel()
  }

  private[this] def putAndBroadcastTask(source: Observable[BroadcastRequest]): Task[Unit] = {
    val queue = AsyncQueue.bounded[BroadcastRequest](settings.maxQueueSize)

    val produceTask = source.observeOn(scheduler).foreachL { req =>
      val queueResult = queue.tryOffer(req)
      req.queuePromise.trySuccess(queueResult)
      if (!queueResult) req.putPromise.tryFailure(new RuntimeException("UTX queue overflow"))
    }

    def createQueueObs[T](queue: AsyncQueue[T]): Observable[T] =
      Observable
        .fromFuture(queue.poll())
        .concatMap(result => Observable(result) ++ createQueueObs(queue))

    val consumeTask = createQueueObs(queue)
      .observeOn(scheduler)
      .mapParallelUnordered(settings.parallelism) {
        case BroadcastRequest(transaction, sender, _, putPromise, forceBroadcast) =>
          Task {
            val value = concurrent.blocking(utx.putIfNew(transaction))

            value.resultE.toOption.filter(_ || forceBroadcast) match {
              case result @ Some(_) =>
                log.trace(s"Broadcasting ${transaction.id()} to ${allChannels.size()} peers except $sender")
                allChannels
                  .write(transaction, (_: Channel) != sender)
                  .addListener((_: ChannelGroupFuture) => putPromise.success(value))
                result

              case None =>
                putPromise.trySuccess(value)
                None
            }
          }.onErrorHandle { error =>
            putPromise.tryFailure(error)
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
        case BroadcastRequest(tx, _, queuePromise, putPromise, forceBroadcast) =>
          var isNew = false
          knownTransactions.get(tx.id(), { () =>
            isNew = true; dummy
          })
          val result = isNew || forceBroadcast
          if (!result) {
            queuePromise.trySuccess(false)
            putPromise.trySuccess(Right(false))
          }
          result
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

object UtxPoolSynchronizer extends ScorexLogging {
  type TxAddResult = TracedResult[ValidationError, Boolean]

  def apply(utx: UtxPool, settings: UtxSynchronizerSettings, allChannels: ChannelGroup, blockSource: Observable[_])(
      implicit sc: Scheduler): UtxPoolSynchronizer = new UtxPoolSynchronizerImpl(utx, settings, allChannels, blockSource)

  implicit class UtxPoolSynchronizerExt(ups: UtxPoolSynchronizer)(implicit sc: Scheduler) {
    def publishTransactions(obs: ChannelObservable[Transaction]): CancelableFuture[Unit] =
      obs
        .consumeWith(Consumer.foreachParallelTask(ups.settings.parallelism) {
          case (channel, tx) =>
            Task
              .deferFuture(ups.queueAndPublishTransaction(tx, channel)._1)
              .map(_ => ())
        })
        .runAsyncLogErr
  }
}
