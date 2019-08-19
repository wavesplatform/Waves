package com.wavesplatform.network

import cats.instances.int._
import com.google.common.cache.CacheBuilder
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.SynchronizationSettings.UtxSynchronizerSettings
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{LastBlockInfo, Transaction}
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.utx.UtxPool
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.execution.{AsyncQueue, CancelableFuture, Scheduler}
import monix.reactive.Observable
import com.wavesplatform.utils.Tap

trait UtxPoolSynchronizer {
  def tryPublish(tx: Transaction, source: Channel): Unit
  def publish(tx: Transaction): TracedResult[ValidationError, Boolean]
}

class UtxPoolSynchronizerImpl(utx: UtxPool, val settings: UtxSynchronizerSettings, allChannels: ChannelGroup, blockSource: Observable[LastBlockInfo])(
    implicit val scheduler: Scheduler
) extends UtxPoolSynchronizer
    with AutoCloseable {

  val queue = AsyncQueue.bounded[(Transaction, Channel)](settings.maxQueueSize)

  val dummy = new Object()
  val knownTransactions = CacheBuilder
    .newBuilder()
    .maximumSize(settings.networkTxCacheSize)
    .build[ByteStr, Object]

  val cancelableFuture = pollTransactions()

  blockSource.map(_.height).distinctUntilChanged.foreach(_ => knownTransactions.invalidateAll())

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

  override def publish(tx: Transaction): TracedResult[ValidationError, Boolean] = {
    utx.putIfNew(tx).tap(_.resultE.foreach(isNew => if (isNew || settings.allowTxRebroadcasting) allChannels.broadcast(tx)))
  }

  override def close(): Unit = cancelableFuture.cancel()

  private def pollTransactions(): CancelableFuture[Unit] = queue.poll().flatMap { case (tx, source) =>
    scheduler.execute { () =>
      utx.putIfNew(tx).resultE match {
        case Right(true) => allChannels.broadcast(tx, Some(source))
        case _ => // either tx is not new or is invalid
      }
    }
    pollTransactions()
  }
}

object UtxPoolSynchronizer extends ScorexLogging {
  def apply(utx: UtxPool, settings: UtxSynchronizerSettings, allChannels: ChannelGroup, blockSource: Observable[LastBlockInfo])(
      implicit sc: Scheduler
  ): UtxPoolSynchronizer = new UtxPoolSynchronizerImpl(utx, settings, allChannels, blockSource)

}
