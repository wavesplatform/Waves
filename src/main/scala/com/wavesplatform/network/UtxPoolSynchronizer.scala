package com.wavesplatform.network

import java.util.concurrent.TimeUnit

import com.google.common.cache.CacheBuilder
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings.SynchronizationSettings.UtxSynchronizerSettings
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.utx.UtxPool
import io.netty.channel.Channel
import io.netty.channel.group.{ChannelGroup, ChannelMatcher}
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.OverflowStrategy

import scala.util.control.NonFatal
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

    val synchronizerFuture = txSource
      .observeOn(scheduler)
      .whileBusyBuffer(OverflowStrategy.DropOldAndSignal(settings.maxQueueSize, { dropped =>
        log.warn(s"UTX queue overflow: $dropped transactions dropped")
        None
      }))
      .bufferTimedWithPressure(settings.maxBufferTime, settings.maxBufferSize)
      .foreach { txBuffer =>
        val toAdd = txBuffer.filter {
          case (_, tx) =>
            val isNew = Option(knownTransactions.getIfPresent(tx.id())).isEmpty
            if (isNew) knownTransactions.put(tx.id(), dummy)
            isNew
        }

        if (toAdd.nonEmpty) {
          toAdd
            .groupBy { case (channel, _) => channel }
            .foreach {
              case (sender, xs) =>
                val channelMatcher: ChannelMatcher = { (_: Channel) != sender }
                xs.foreach {
                  case (_, tx) =>
                    utx.putIfNew(tx) match {
                      case Right((true, _)) =>
                        allChannels.write(RawBytes.from(tx), channelMatcher)

                      case Left(error)  =>
                        log.debug(s"Error adding transaction to UTX pool: $error")

                      case _ =>
                        // Ignore
                    }
                }
            }
          allChannels.flush()
        }
      }

    synchronizerFuture.onComplete {
      case Success(_)            => log.error("UtxPoolSynschronizer stops")
      case Failure(NonFatal(th)) => log.error("Error in utx pool synchronizer", th)
    }
    synchronizerFuture
  }
}
