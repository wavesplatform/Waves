package com.wavesplatform.settings

import scala.concurrent.duration.FiniteDuration

case class UtxSettings(maxSize: Int,
                       maxBytesSize: Long,
                       blacklistSenderAddresses: Set[String],
                       allowBlacklistedTransferTo: Set[String],
                       cleanupInterval: FiniteDuration,
                       allowTransactionsFromSmartAccounts: Boolean)
