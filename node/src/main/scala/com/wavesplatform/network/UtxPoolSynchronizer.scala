package com.wavesplatform.network

import cats.instances.int._
import com.google.common.cache.CacheBuilder
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.UtxPoolSynchronizer.TxAddResult
import com.wavesplatform.settings.SynchronizationSettings.UtxSynchronizerSettings
import com.wavesplatform.transaction.{LastBlockInfo, Transaction}
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.utx.UtxPool
import io.netty.channel.Channel
import io.netty.channel.group.{ChannelGroup, ChannelGroupFuture}
import monix.execution.{AsyncQueue, CancelableFuture, Scheduler}
import monix.reactive.Observable

import scala.concurrent.{Future, Promise}

trait UtxPoolSynchronizer {
  def tryPublish(tx: Transaction, source: Channel): Unit
  def publish(tx: Transaction): Future[TxAddResult]
}

class UtxPoolSynchronizerImpl(utx: UtxPool, val settings: UtxSynchronizerSettings, allChannels: ChannelGroup, blockSource: Observable[LastBlockInfo])(
    implicit val scheduler: Scheduler
) extends UtxPoolSynchronizer
    with AutoCloseable {

  import UtxPoolSynchronizer._

  val queue = AsyncQueue.bounded[BroadcastRequest](settings.maxQueueSize)

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
    queue.tryOffer(BroadcastRequest(tx, Some(source), Promise(), forceBroadcast = false))
  }

  override def publish(tx: Transaction): Future[TxAddResult] = {
    val p = Promise[TxAddResult]
    queue.offer(BroadcastRequest(tx, None, p, settings.allowTxRebroadcasting))
    p.future
  }

  override def close(): Unit = cancelableFuture.cancel()

  private def pollTransactions(): CancelableFuture[Unit] = queue.poll().flatMap { req =>
    scheduler.execute { () =>
      val result = utx.putIfNew(req.transaction)
      req.putPromise.trySuccess(result.resultE.map(_ => req.transaction))
      result.resultE match {
        case Right(isNew) =>
          if (isNew || req.forceBroadcast) {
            allChannels
              .write(req.transaction, !req.source.contains(_))
              .addListener((_: ChannelGroupFuture) => req.putPromise.trySuccess(Right(req.transaction)))
          } else req.putPromise.trySuccess(Right(req.transaction))
        case Left(error) => req.putPromise.trySuccess(Left(error))
      }
    }

    pollTransactions()
  }
}

object UtxPoolSynchronizer extends ScorexLogging {
  case class BroadcastRequest(transaction: Transaction, source: Option[Channel], putPromise: Promise[TxAddResult], forceBroadcast: Boolean)

  type TxAddResult = Either[ValidationError, Transaction]

  def apply(utx: UtxPool, settings: UtxSynchronizerSettings, allChannels: ChannelGroup, blockSource: Observable[LastBlockInfo])(
      implicit sc: Scheduler
  ): UtxPoolSynchronizer = new UtxPoolSynchronizerImpl(utx, settings, allChannels, blockSource)

}
