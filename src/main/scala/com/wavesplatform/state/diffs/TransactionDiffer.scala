package com.wavesplatform.state.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import scorex.transaction._
import scorex.transaction.smart.Verifier
import scorex.transaction.validation.ValidationError

object TransactionDiffer {

  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError

  def apply(settings: FunctionalitySettings, prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)(
      blockchain: Blockchain,
      tx: Transaction): Either[ValidationError, Diff] = {
    for {
      _            <- Verifier(blockchain, currentBlockHeight)(tx)
      _            <- CommonValidation.disallowTxFromFuture(settings, currentBlockTimestamp, tx)
      _            <- CommonValidation.disallowTxFromPast(prevBlockTimestamp, tx)
      _            <- CommonValidation.disallowBeforeActivationTime(blockchain, currentBlockHeight, tx)
      _            <- CommonValidation.disallowDuplicateIds(blockchain, settings, currentBlockHeight, tx)
      _            <- CommonValidation.disallowSendingGreaterThanBalance(blockchain, settings, currentBlockTimestamp, tx)
      _            <- CommonValidation.checkFee(blockchain, settings, currentBlockHeight, tx)
      diff         <- ModernTransactionDiff(settings, prevBlockTimestamp, currentBlockTimestamp, currentBlockHeight)(blockchain, tx)
      positiveDiff <- BalanceDiffValidation(blockchain, currentBlockHeight, settings)(diff)
    } yield positiveDiff
  }.left.map(TransactionValidationError(_, tx))
}
