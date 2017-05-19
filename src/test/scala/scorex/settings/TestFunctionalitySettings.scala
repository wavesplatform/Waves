package scorex.settings

import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings, GenesisTransactionSettings}
import scala.concurrent.duration._

object TestFunctionalitySettings {
  val Enabled = FunctionalitySettings(
    allowTemporaryNegativeUntil = 0L, allowInvalidPaymentTransactionsByTimestamp = 0L,
    requireSortedTransactionsAfter = 0L, generatingBalanceDepthFrom50To1000AfterHeight = 0L,
    minimalGeneratingBalanceAfterTimestamp = 0L,
    allowTransactionsFromFutureUntil = Long.MaxValue, allowUnissuedAssetsUntil = 0L,
    allowBurnTransactionAfterTimestamp = 0L, allowLeaseTransactionAfterTimestamp = 0L,
    requirePaymentUniqueId = 0L, allowExchangeTransactionAfterTimestamp = 0L,
    allowInvalidReissueInSameBlockUntilTimestamp = 0L, allowCreateAliasTransactionAfterTimestamp = 0L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 0L, resetEffectiveBalancesAtHeight = 0,
    allowMakeAssetNameUniqueTransactionAfterTimestamp = 0L,
    allowTransferLeasedBalanceUntil = 0L
  )
}