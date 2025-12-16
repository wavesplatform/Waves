package com.wavesplatform.state.diffs

import cats.syntax.either.*
import com.wavesplatform.consensus.GeneratingBalanceProvider
import com.wavesplatform.crypto.bls.BlsUtils
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
      _ <- Either.raiseUnless(tx.generationPeriodStart == next.start) {
        GenericError(s"Expected the next period start height (${next.start}), got ${tx.generationPeriodStart}")
      }
      _ <- Either.raiseUnless(
        BlsUtils.verifyBasic(tx.commitmentSignature.arr, tx.endorserPublicKey.arr ++ tx.generationPeriodStart.toByteArray, tx.endorserPublicKey.arr)
      )(GenericError("Invalid commitment signature"))
      _ <- blockchain.committedGenerators(next).foldLeft(Either.unit[GenericError]) {
        case (r @ Left(_), _) => r
        case (Right(_), (addr, blsPk)) =>
          if (addr == sender) GenericError(s"$sender is already committed").asLeft
          else if (blsPk == tx.endorserPublicKey) GenericError(s"BLS key ${tx.endorserPublicKey} is already committed, try another key").asLeft
          else ().asRight
      }
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
      generatingBalanceAfterDeposit = SnapshotBlockchain(blockchain, snapshot).generatingBalance(sender)
      _ <- Either.raiseUnless(generatingBalanceAfterDeposit >= GeneratingBalanceProvider.MinimalEffectiveBalanceForGenerator2)(
        GenericError(
          s"Generating balance $generatingBalanceAfterDeposit is less than ${GeneratingBalanceProvider.MinimalEffectiveBalanceForGenerator2} required for block generation"
        )
      )
    } yield snapshot
  }
}
