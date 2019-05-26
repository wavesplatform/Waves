package com.wavesplatform.settings

case class UtxSettings(maxSize: Int,
                       maxBytesSize: Long,
                       maxScriptedSize: Int,
                       blacklistSenderAddresses: Set[String],
                       allowRebroadcasting: Boolean,
                       allowBlacklistedTransferTo: Set[String],
                       allowTransactionsFromSmartAccounts: Boolean,
                       allowSkipChecks: Boolean)
