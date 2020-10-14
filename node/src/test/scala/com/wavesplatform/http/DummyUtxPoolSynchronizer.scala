package com.wavesplatform.http
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import io.netty.channel.Channel

import scala.concurrent.Future

object DummyUtxPoolSynchronizer {
  val accepting: UtxPoolSynchronizer = new UtxPoolSynchronizer {
    override def tryPublish(tx: Transaction, source: Channel): Unit = ()

    override def publish(tx: Transaction): Future[TracedResult[ValidationError, Boolean]] =
      Future.successful(TracedResult(Right(true)))
  }

  def rejecting(error: Transaction => ValidationError): UtxPoolSynchronizer = new UtxPoolSynchronizer {
    override def tryPublish(tx: Transaction, source: Channel): Unit = ()

    override def publish(tx: Transaction): Future[TracedResult[ValidationError, Boolean]] =
      Future.successful(TracedResult(Left(error(tx))))
  }
}
