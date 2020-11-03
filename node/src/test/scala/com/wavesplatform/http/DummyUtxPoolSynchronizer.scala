package com.wavesplatform.http

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult

import scala.concurrent.Future

object DummyUtxPoolSynchronizer {
  val accepting: UtxPoolSynchronizer = { (_, _) =>
    Future.successful(TracedResult(Right(true)))
  }

  def rejecting(error: Transaction => ValidationError): UtxPoolSynchronizer = { (tx, _) =>
    Future.successful(TracedResult(Left(error(tx))))
  }
}
