package com.wavesplatform.settings

case class UtxSettings(maxSize: Int,
                       maxBytesSize: Long,
                       blacklistSenderAddresses: Set[String],
                       allowBlacklistedTransferTo: Set[String],
                       allowTransactionsFromSmartAccounts: Boolean)
