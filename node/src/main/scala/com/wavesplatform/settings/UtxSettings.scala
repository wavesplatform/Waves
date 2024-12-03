package com.wavesplatform.settings

import pureconfig.*
import pureconfig.generic.derivation.default.*

case class UtxSettings(
    maxSize: Int,
    maxBytesSize: Long,
    maxScriptedSize: Int,
    blacklistSenderAddresses: Set[String],
    allowBlacklistedTransferTo: Set[String],
    fastLaneAddresses: Set[String],
    allowTransactionsFromSmartAccounts: Boolean,
    allowSkipChecks: Boolean,
    forceValidateInCleanup: Boolean,
    alwaysUnlimitedExecution: Boolean
) derives ConfigReader
