package com.wavesplatform.http
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.transaction.Transaction
import io.netty.channel.Channel

import scala.concurrent.Future

object DummyUtxPoolSynchronizer {
  val accepting: UtxPoolSynchronizer = new UtxPoolSynchronizer {
    override def tryPublish(tx: Transaction, source: Channel): Unit                = {}
    override def publish(tx: Transaction): Future[UtxPoolSynchronizer.TxAddResult] = Future.successful(Right(tx))
  }

  def rejecting(error: Transaction => ValidationError): UtxPoolSynchronizer = new UtxPoolSynchronizer {
    override def tryPublish(tx: Transaction, source: Channel): Unit                = {}
    override def publish(tx: Transaction): Future[UtxPoolSynchronizer.TxAddResult] = Future.successful(Left(error(tx)))
  }
}
