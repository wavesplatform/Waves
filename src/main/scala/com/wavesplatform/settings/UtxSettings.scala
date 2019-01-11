package com.wavesplatform.settings

import scala.concurrent.duration.FiniteDuration

case class UtxSettings(maxSize: Int,
                       blacklistSenderAddresses: Set[String],
                       allowBlacklistedTransferTo: Set[String],
                       cleanupInterval: FiniteDuration,
                       allowTransactionsFromSmartAccounts: Boolean)
