package com.wavesplatform.network

import java.util.concurrent.TimeUnit

import com.google.common.cache.CacheBuilder
import com.wavesplatform.UtxPool
import com.wavesplatform.settings.SynchronizationSettings.UtxSynchronizerSettings
import com.wavesplatform.state2.ByteStr
import io.netty.channel.group.ChannelGroup
import monix.execution.Scheduler
import monix.reactive.Observable
import scorex.transaction.Transaction

object UtxPoolSynchronizer {
  def apply(utx: UtxPool, utxSynchronizerSettings: UtxSynchronizerSettings, allChannels: ChannelGroup, txs: ChannelObservable[Transaction]): Observable[Unit] = {

    implicit val scheduler: Scheduler = Scheduler.singleThread("utx-pool-sync")

    val dummy = new Object()

    val knownTransactions = CacheBuilder
      .newBuilder()
      .maximumSize(utxSynchronizerSettings.networkTxCacheSize)
      .expireAfterWrite(utxSynchronizerSettings.networkTxCacheTime.toMillis, TimeUnit.MILLISECONDS)
      .build[ByteStr, Object]

    txs.observeOn(scheduler).map { case (channel, tx) => knownTransactions.get(tx.id(), () => {
      utx.putIfNew(tx).map(_ => allChannels.broadcast(tx, Some(channel)))
      dummy
    })
    }
  }
}