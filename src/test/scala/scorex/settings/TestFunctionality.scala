package scorex.settings

import com.wavesplatform.features.{FeatureProvider, FeatureStatus, Functionalities}
import com.wavesplatform.settings.FunctionalitySettings

object TestFunctionality {

  val EnabledSettings = FunctionalitySettings(
    allowTemporaryNegativeUntil = 0L, allowInvalidPaymentTransactionsByTimestamp = 0L,
    requireSortedTransactionsAfter = 0L, generationBalanceDepthFrom50To1000AfterHeight = 0L,
    minimalGeneratingBalanceAfter = 0L,
    allowTransactionsFromFutureUntil = Long.MaxValue, allowUnissuedAssetsUntil = 0L,
    allowBurnTransactionAfter = 0L, allowLeaseTransactionAfter = 0L,
    requirePaymentUniqueIdAfter = 0L, allowExchangeTransactionAfter = 0L,
    allowInvalidReissueInSameBlockUntilTimestamp = 0L, allowCreatealiasTransactionAfter = 0L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 0L, resetEffectiveBalancesAtHeight = 0,
    allowLeasedBalanceTransferUntil = 0L,
    featureCheckBlocksPeriod = 100, blocksForFeatureActivation = 90
  )

  val EnabledProvider: FeatureProvider = (_: Short) => FeatureStatus.Activated

  val EnabledFunctionalities: Functionalities = new Functionalities(EnabledSettings, EnabledProvider)
}
