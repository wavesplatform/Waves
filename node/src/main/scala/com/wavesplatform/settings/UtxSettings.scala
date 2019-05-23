package com.wavesplatform.settings

import scala.concurrent.duration.FiniteDuration

case class UtxSettings(maxSize: Int,
                       maxBytesSize: Long,
                       maxScriptedSize: Int,
                       blacklistSenderAddresses: Set[String],
                       allowRebroadcasting: Boolean,
                       allowBlacklistedTransferTo: Set[String],
                       allowTransactionsFromSmartAccounts: Boolean,
                       allowSkipChecks: Boolean,
                       maxPackTime: FiniteDuration)
