package com.wavesplatform.state.diffs

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.*
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.{Asset, CommitToGenerationTransaction}

import scala.util.Either.cond

object CommitToGenerationTransactionDiff {
  def apply(blockchain: Blockchain)(tx: CommitToGenerationTransaction): Either[ValidationError, StateSnapshot] = {
    val commitmentPeriod         = blockchain.settings.functionalitySettings.commitmentPeriod
    val currentPeriodStartHeight = blockchain.currentGenerationPeriodStartHeight
    val nextPeriodStartHeight    = currentPeriodStartHeight + commitmentPeriod

    for {
      // TODO: Check BLS signature
      _ <- cond(
        tx.generationPeriodStart % commitmentPeriod == 0,
        (),
        GenericError(
          s"Generation period start ${tx.generationPeriodStart} must be a multiple of $commitmentPeriod. " +
            s"Allowed height is $nextPeriodStartHeight"
        )
      )
      _ <- cond(
        tx.generationPeriodStart == nextPeriodStartHeight,
        (),
        GenericError(
          s"Generation period start ${tx.generationPeriodStart} must be on the next period. " +
            s"Current height is ${blockchain.height}, allowed height is $nextPeriodStartHeight"
        )
      )
      snapshot <- StateSnapshot.build(
        blockchain,
        portfolios = Map(tx.sender.toAddress -> Portfolio.build(Asset.Waves -> -tx.fee.value)),
        nextCommittedGenerators = Map(tx.sender -> tx.endorsementPublicKey)
      )
    } yield snapshot
  }
}
