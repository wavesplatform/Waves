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
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.subjects.ConcurrentSubject
import monix.reactive.{Observable, OverflowStrategy}

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

class UtxPoolSynchronizer(utx: UtxPool, settings: UtxSynchronizerSettings, allChannels: ChannelGroup, blockSource: Observable[_])
    extends ScorexLogging {
  implicit lazy val scheduler: Scheduler = Scheduler.forkJoin(settings.parallelism, settings.maxThreads, "utx-pool-sync")
  private[this] lazy val txSource = ConcurrentSubject.publishToOne[BroadcastRequest]
  private[this] var future: CancelableFuture[Unit] = _

  def start(): Unit = synchronized {
    if (future == null) {
      future = start(txSource.publish, blockSource)
      sys.addShutdownHook(future.cancel())
    }
  }

  //noinspection ScalaStyle
  def publishTransaction(tx: Transaction, source: Channel = null, forceBroadcast: Boolean = false): Future[TxAddResult] = {
    val promise = Promise[TxAddResult]
    txSource.onNext(BroadcastRequest(tx, source, promise, forceBroadcast))
    promise.future
  }

  def publishTransactions(obs: ChannelObservable[Transaction]): Task[Unit] = {
    val dp = Promise[TxAddResult]
    obs.foreachL { case (c, t) => txSource.onNext(BroadcastRequest(t, c, dp, forceBroadcast = false)) }
  }

  private[this] def putAndBroadcastTask(source: Observable[BroadcastRequest]): Task[Unit] = {
    source
      .whileBusyBuffer(OverflowStrategy.DropOldAndSignal(settings.maxQueueSize, { dropped =>
        log.warn(s"UTX queue overflow: $dropped transactions dropped")
        None
      }))
      .mapParallelUnordered(settings.parallelism) {
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
      .bufferTimedAndCounted(settings.maxBufferTime, settings.maxBufferSize)
      .filter(_.flatten.nonEmpty)
      .foreachL(_ => allChannels.flush())
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
        case BroadcastRequest(tx, _, _, _) =>
          var isNew = false
          knownTransactions.get(tx.id(), { () =>
            isNew = true; dummy
          })
          isNew
      }

    val synchronizerFuture = putAndBroadcastTask(newTxSource).runAsyncLogErr

    synchronizerFuture.onComplete {
      case Success(_) => log.info("UtxPoolSynschronizer stops")
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
