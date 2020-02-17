package com.wavesplatform.settings

case class UtxSettings(
    maxSize: Int,
    maxBytesSize: Long,
    maxScriptedSize: Int,
    blacklistSenderAddresses: Set[String],
    allowBlacklistedTransferTo: Set[String],
    allowTransactionsFromSmartAccounts: Boolean,
    allowSkipChecks: Boolean
)
