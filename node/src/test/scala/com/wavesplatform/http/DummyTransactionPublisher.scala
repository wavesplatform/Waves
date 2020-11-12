package com.wavesplatform.http

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult

import scala.concurrent.Future

object DummyTransactionPublisher {
  val accepting: TransactionPublisher = { (_, _) =>
    Future.successful(TracedResult(Right(true)))
  }

  def rejecting(error: Transaction => ValidationError): TransactionPublisher = { (tx, _) =>
    Future.successful(TracedResult(Left(error(tx))))
  }
}
