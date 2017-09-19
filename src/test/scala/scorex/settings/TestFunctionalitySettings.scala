package scorex.settings

import com.wavesplatform.settings.FunctionalitySettings

object TestFunctionalitySettings {
  val Enabled = FunctionalitySettings(
    featureCheckBlocksPeriod = 10000,
    blocksForFeatureActivation = 9000,
    allowTemporaryNegativeUntil = 0L, allowInvalidPaymentTransactionsByTimestamp = 0L,
    requireSortedTransactionsAfter = 0L, generationBalanceDepthFrom50To1000AfterHeight = 0L,
    minimalGeneratingBalanceAfter = 0L,
    allowTransactionsFromFutureUntil = Long.MaxValue, allowUnissuedAssetsUntil = 0L,
    allowBurnTransactionAfter = 0L, allowLeaseTransactionAfter = 0L,
    requirePaymentUniqueIdAfter = 0L, allowExchangeTransactionAfter = 0L,
    allowInvalidReissueInSameBlockUntilTimestamp = 0L, allowCreatealiasTransactionAfter = 0L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 0L, resetEffectiveBalancesAtHeight = 0,
    allowLeasedBalanceTransferUntil = 0L
  )

   val Stub = Enabled.copy(featureCheckBlocksPeriod = 100, blocksForFeatureActivation = 90)
}
