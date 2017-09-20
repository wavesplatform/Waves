package com.wavesplatform.features

import com.wavesplatform.settings.FunctionalitySettings


trait Functionality {}

trait TimestampedFunctionality extends Functionality {
  def check(timestamp: Long): Either[InvalidFunctionalityError, Unit]
}

trait HeightDependentFunctionality extends Functionality {
  def check(height: Long): Either[InvalidFunctionalityError, Unit]
}

trait BlockchainFunctionality extends Functionality {
  def check(): Either[InvalidFunctionalityError, Unit]
}


class Functionalities(settings: FunctionalitySettings, provider: FeatureProvider) {

  import Functionalities._

  val allowTemporaryNegativeUntil: TimestampedFunctionality = (ts: Long) => allowUntil(ts, settings.allowTemporaryNegativeUntil)
  //  val allowInvalidPaymentTransactionsByTimestamp: TimestampedFunctionality = (ts: Long) => ts < settings.allowInvalidPaymentTransactionsByTimestamp
  val allowUnsortedTransactionsUntil: TimestampedFunctionality = (ts: Long) => allowUntil(ts, settings.requireSortedTransactionsAfter)
  val deepGenerationBalanceDepthAfter: HeightDependentFunctionality = (h: Long) =>
    if (h >= settings.generationBalanceDepthFrom50To1000AfterHeight) Right(Unit)
    else Left(InvalidFunctionalityError(s"not available after height=${settings.generationBalanceDepthFrom50To1000AfterHeight}"))
  val minimalGeneratingBalanceAfter: TimestampedFunctionality = (ts: Long) => requiredAfter(ts, settings.minimalGeneratingBalanceAfter)
  val allowTransactionsFromFutureUntil: TimestampedFunctionality = (ts: Long) => allowUntil(ts, settings.allowTransactionsFromFutureUntil)
  val allowUnissuedAssetsUpTo: TimestampedFunctionality = (ts: Long) => allowUpTo(ts, settings.allowUnissuedAssetsUntil)
  val allowBurnTransactionAfter: TimestampedFunctionality = (ts: Long) => allowAfter(ts, settings.allowBurnTransactionAfter)
  val allowLeaseTransactionAfter: TimestampedFunctionality = (ts: Long) => allowAfter(ts, settings.allowLeaseTransactionAfter)
  val requirePaymentUniqueIdAfter: TimestampedFunctionality = (ts: Long) => requiredAfter(ts, settings.requirePaymentUniqueIdAfter)
  val allowExchangeTransactionAfter: TimestampedFunctionality = (ts: Long) => allowAfter(ts, settings.allowExchangeTransactionAfter)
  val allowInvalidReissueInSameBlockUpTo: TimestampedFunctionality = (ts: Long) => allowUpTo(ts, settings.allowInvalidReissueInSameBlockUntilTimestamp)
  val allowCreateAliasTransactionAfter: TimestampedFunctionality = (ts: Long) => allowAfter(ts, settings.allowCreatealiasTransactionAfter)
  val allowMultipleLeaseCancelTransactionUntil: TimestampedFunctionality = (ts: Long) => allowUntil(ts, settings.allowMultipleLeaseCancelTransactionUntilTimestamp)
  val resetEffectiveBalancesAt: HeightDependentFunctionality = (h: Long) =>
    if (h == settings.resetEffectiveBalancesAtHeight) Right(Unit)
    else Left(InvalidFunctionalityError(s"not available at height=$h"))
  val allowLeasedBalanceTransferUpTo: TimestampedFunctionality = (ts: Long) => allowUpTo(ts, settings.allowLeasedBalanceTransferUntil)
}

object Functionalities {

  private def allowUntil(timestamp: Long, expectation: Long): Either[InvalidFunctionalityError, Unit] = {
    if (timestamp < expectation) Right(Unit) else Left(InvalidFunctionalityError.allowUntilTimestamp(expectation))
  }

  private def allowUpTo(timestamp: Long, expectation: Long): Either[InvalidFunctionalityError, Unit] = {
    if (timestamp <= expectation) Right(Unit) else Left(InvalidFunctionalityError.allowUntilTimestamp(expectation))
  }

  private def allowAfter(timestamp: Long, expectation: Long): Either[InvalidFunctionalityError, Unit] = {
    if (timestamp > expectation) Right(Unit) else Left(InvalidFunctionalityError.allowAfterTimestamp(expectation))
  }

  private def requiredAfter(timestamp: Long, expectation: Long): Either[InvalidFunctionalityError, Unit] = {
    if (timestamp >= expectation) Right(Unit) else Left(InvalidFunctionalityError.requiredAfterTimestamp(expectation))
  }

}
