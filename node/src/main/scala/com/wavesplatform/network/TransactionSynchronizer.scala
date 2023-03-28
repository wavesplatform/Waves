package com.wavesplatform.network

import com.google.common.cache.CacheBuilder
import com.typesafe.scalalogging.LazyLogging
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings.SynchronizationSettings.UtxSynchronizerSettings
import com.wavesplatform.transaction.Transaction
import io.netty.channel.Channel
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.{Observable, OverflowStrategy}

object TransactionSynchronizer extends LazyLogging {
  def apply(
      settings: UtxSynchronizerSettings,
      lastBlockId: Observable[ByteStr],
      transactions: Observable[(Channel, Transaction)],
      transactionValidator: TransactionPublisher
  )(implicit scheduler: Scheduler): Cancelable = {
    val dummy = new Object()
    val knownTransactions = CacheBuilder
      .newBuilder()
      .maximumSize(settings.networkTxCacheSize)
      .build[ByteStr, Object]

    lastBlockId.foreach { id =>
      logger.trace(s"Invalidating known transactions for block id $id")
      knownTransactions.invalidateAll()
    }

    def transactionIsNew(txId: ByteStr): Boolean = {
      var isNew = false
      knownTransactions.get(
        txId,
        { () =>
          isNew = true; dummy
        }
      )
      isNew
    }

    transactions
      .filter { case (_, tx) =>
        transactionIsNew(tx.id())
      }
      .whileBusyBuffer(OverflowStrategy.DropNew(settings.maxQueueSize))
      .mapParallelUnorderedF(settings.maxThreads) { case (channel, tx) =>
        transactionValidator.validateAndBroadcast(tx, Some(channel))
      }
      .subscribe()
  }
}
