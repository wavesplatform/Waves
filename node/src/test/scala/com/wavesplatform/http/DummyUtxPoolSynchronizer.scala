package com.wavesplatform.http

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.TransactionValidator
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult

import scala.concurrent.Future

object DummyUtxPoolSynchronizer {
  val accepting: TransactionValidator = { (_, _) =>
    Future.successful(TracedResult(Right(true)))
  }

  def rejecting(error: Transaction => ValidationError): TransactionValidator = { (tx, _) =>
    Future.successful(TracedResult(Left(error(tx))))
  }
}
