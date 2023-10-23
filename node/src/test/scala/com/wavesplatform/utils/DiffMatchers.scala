package com.wavesplatform.utils

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.StateSnapshot
import com.wavesplatform.state.TxMeta.Status.Succeeded
import org.scalatest.matchers.{MatchResult, Matcher}

trait DiffMatchers {
  def containAppliedTx(transactionId: ByteStr) = new DiffAppliedTxMatcher(transactionId, true)
  def containFailedTx(transactionId: ByteStr)  = new DiffAppliedTxMatcher(transactionId, false)

  class DiffAppliedTxMatcher(transactionId: ByteStr, shouldBeApplied: Boolean) extends Matcher[StateSnapshot] {
    override def apply(diff: StateSnapshot): MatchResult = {
      val isApplied = diff.transactions.get(transactionId) match {
        case Some(nt) if nt.status == Succeeded => true
        case _                                  => false
      }
      MatchResult(
        shouldBeApplied == isApplied,
        s"$transactionId was not ${if (shouldBeApplied) "applied" else "failed"}: $diff",
        s"$transactionId was ${if (shouldBeApplied) "applied" else "failed"}: $diff"
      )
    }
  }
}
