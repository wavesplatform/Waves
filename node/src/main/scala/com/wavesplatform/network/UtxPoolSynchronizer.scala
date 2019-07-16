package com.wavesplatform.network

import com.google.common.cache.CacheBuilder
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.UtxPoolSynchronizer.{BroadcastRequest, TxAddResult}
import com.wavesplatform.settings.SynchronizationSettings.UtxSynchronizerSettings
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.utx.UtxPool
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.eval.Task
import monix.execution.{AsyncQueue, CancelableFuture, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

class UtxPoolSynchronizer(utx: UtxPool, settings: UtxSynchronizerSettings, allChannels: ChannelGroup, blockSource: Observable[_])(
    implicit val scheduler: Scheduler = Scheduler.global)
    extends ScorexLogging {

  private[this] lazy val txSource = ConcurrentSubject.publishToOne[BroadcastRequest]

  start(txSource, blockSource)

  //noinspection ScalaStyle
  def publishTransaction(tx: Transaction, source: Channel = null, forceBroadcast: Boolean = false): Future[TxAddResult] = {
    val promise = Promise[TxAddResult]
    txSource.onNext(BroadcastRequest(tx, source, promise, forceBroadcast))
    promise.future
  }

  def publishTransactions(obs: ChannelObservable[Transaction]): CancelableFuture[Unit] = {
    val dp = Promise[TxAddResult]
    obs.foreach { case (c, t) => txSource.onNext(BroadcastRequest(t, c, dp, forceBroadcast = false)) }
  }

  private[this] def putAndBroadcastTask(source: Observable[BroadcastRequest]): Task[Unit] = {
    val queue = AsyncQueue.bounded[BroadcastRequest](settings.maxQueueSize)

    val produceTask = source.foreachL { req =>
      if (!queue.tryOffer(req))
        req.promise.tryFailure(new RuntimeException("UTX queue overflow"))
    }

    val consumeTask = Observable
      .repeatEval(queue.poll())
      .mapParallelUnordered(settings.parallelism) { futureReq =>
        Task.fromFuture(futureReq).flatMap {
          case BroadcastRequest(transaction, sender, promise, forceBroadcast) =>
            Task {
              val value = concurrent.blocking(utx.putIfNew(transaction))
              promise.trySuccess(value)

              value.resultE.toOption.filter(_ || forceBroadcast).map { _ =>
                log.trace(s"Broadcasting ${transaction.id()} to ${allChannels.size()} peers except $sender")
                allChannels.write(transaction, (_: Channel) != sender)
              }
            }.onErrorHandle { error =>
              promise.tryFailure(error)
              None
            }
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
    synchronizerFuture
  }
}

object UtxPoolSynchronizer {
  type TxAddResult = TracedResult[ValidationError, Boolean]
  private final case class BroadcastRequest(transaction: Transaction, source: Channel, promise: Promise[TxAddResult], forceBroadcast: Boolean)
}
