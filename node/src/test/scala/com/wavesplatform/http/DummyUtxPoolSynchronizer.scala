package com.wavesplatform.http
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import io.netty.channel.Channel

object DummyUtxPoolSynchronizer {
  val accepting: UtxPoolSynchronizer = new UtxPoolSynchronizer {
    override def tryPublish(tx: Transaction, source: Channel): Unit               = {}
    override def publish(tx: Transaction): TracedResult[ValidationError, Boolean] = TracedResult(Right(true))
  }

  def rejecting(error: Transaction => ValidationError): UtxPoolSynchronizer = new UtxPoolSynchronizer {
    override def tryPublish(tx: Transaction, source: Channel): Unit               = {}
    override def publish(tx: Transaction): TracedResult[ValidationError, Boolean] = TracedResult(Left(error(tx)))
  }
}
