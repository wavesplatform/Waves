package com.wavesplatform.network

import java.util.concurrent.TimeUnit

import com.google.common.cache.CacheBuilder
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings.SynchronizationSettings.UtxSynchronizerSettings
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.utx.UtxPool
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.{Consumer, OverflowStrategy}

import scala.util.{Failure, Success}

object UtxPoolSynchronizer extends ScorexLogging {

  def start(utx: UtxPool,
            settings: UtxSynchronizerSettings,
            allChannels: ChannelGroup,
            txSource: ChannelObservable[Transaction]): CancelableFuture[Unit] = {
    implicit val scheduler: Scheduler = Scheduler.forkJoin(settings.parallelism, settings.maxThreads, "utx-pool-sync")

    val dummy = new Object()
    val knownTransactions = CacheBuilder
      .newBuilder()
      .maximumSize(settings.networkTxCacheSize)
      .expireAfterWrite(settings.networkTxCacheTime.toMillis, TimeUnit.MILLISECONDS)
      .build[ByteStr, Object]

    val newTxSource = txSource
      .observeOn(scheduler)
      .whileBusyBuffer(OverflowStrategy.DropOldAndSignal(settings.maxQueueSize, { dropped =>
        log.warn(s"UTX queue overflow: $dropped transactions dropped")
        None
      }))
      .filter { case (_, tx) =>
        var isNew = false
        knownTransactions.get(tx.id(), { () => isNew = true; dummy })
        isNew
      }

    val putIfNewAndBroadcast = Consumer.foreachParallel[(Channel, Transaction)](settings.parallelism) { case (sender, transaction) =>
      utx.putIfNew(transaction) match {
        case Right((isNew, _)) =>
          if (isNew) allChannels.write(RawBytes.from(transaction), (_: Channel) != sender)

        case Left(error) =>
          log.debug(s"Error adding transaction to UTX pool: $error")
      }
    }

    val synchronizerFuture = newTxSource
      .consumeWith(putIfNewAndBroadcast)
      .runAsyncLogErr
      .flatMap { _ =>
        newTxSource
          .map(_ => 1)
          .bufferTimedAndCounted(settings.maxBufferTime, settings.maxBufferSize)
          .filter(_.nonEmpty)
          .foreachL(_ => allChannels.flush())
          .runAsyncLogErr
      }

    synchronizerFuture.onComplete {
      case Success(_)     => log.error("UtxPoolSynschronizer stops")
      case Failure(error) => log.error("Error in utx pool synchronizer", error)
    }

    synchronizerFuture
  }
}
