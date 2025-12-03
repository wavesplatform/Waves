package com.wavesplatform.state.diffs

import cats.syntax.either.*
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.*
import com.wavesplatform.transaction.CommitToGenerationTransaction
import com.wavesplatform.transaction.TxValidationError.{ActivationError, GenericError}

object CommitToGenerationTransactionDiff {
  def apply(blockchain: Blockchain)(tx: CommitToGenerationTransaction): Either[ValidationError, StateSnapshot] = {
    val sender = tx.sender.toAddress

    for {
      current <- blockchain.currentGenerationPeriod.toRight(ActivationError("DeterministicFinality is not yet activated"))
      next = current.next
      // TODO: Check BLS signature
      _ <- Either.raiseUnless(tx.generationPeriodStart == next.start) {
        GenericError(s"Expected the next period start height (${next.start}), got ${tx.generationPeriodStart}")
      }
      committed = blockchain.committedGenerators(next).map { case (address, _) => address }.toSet
      _ <- Either.raiseWhen(committed.contains(sender)) { GenericError(s"$sender is already committed") }
      snapshot <- StateSnapshot.build(
        blockchain,
        portfolios = Map(
          sender -> Portfolio(
            balance = -tx.fee.value
            // generationDeposit = ??? // We don't need this, because calculate from nextCommittedGenerators
          )
        ),
        nextCommittedGenerators = Seq(tx.sender -> tx.endorserPublicKey)
      )
    } yield snapshot
  }
}
