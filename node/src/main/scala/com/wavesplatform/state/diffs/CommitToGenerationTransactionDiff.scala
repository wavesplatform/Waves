package com.wavesplatform.state.diffs

import cats.syntax.either.*
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.*
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.{Asset, CommitToGenerationTransaction}

object CommitToGenerationTransactionDiff {
  def apply(blockchain: Blockchain)(tx: CommitToGenerationTransaction): Either[ValidationError, StateSnapshot] = {
    val current = blockchain.currentGenerationPeriod
    val next    = current.next

    for {
      // TODO: Check BLS signature
      _ <- Either.raiseUnless(tx.generationPeriodStart % current.period == 0) {
        GenericError(
          s"Generation period start ${tx.generationPeriodStart} must be a multiple of ${current.period}. Allowed height is $next"
        )
      }
      _ <- Either.raiseUnless(tx.generationPeriodStart == next.start) {
        GenericError(s"Expected the next period start height (${next.start}), got ${tx.generationPeriodStart}")
      }
      snapshot <- StateSnapshot.build(
        blockchain,
        portfolios = Map(tx.sender.toAddress -> Portfolio.build(Asset.Waves -> -tx.fee.value)),
        nextCommittedGenerators = Seq((tx.sender.toAddress, tx.endorsementPublicKey, TransactionId(tx.id())))
      )
    } yield snapshot
  }
}
