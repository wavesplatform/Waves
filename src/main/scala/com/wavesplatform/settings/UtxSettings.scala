package com.wavesplatform.settings

import scala.concurrent.duration.FiniteDuration

case class UtxSettings(maxSize: Int, maxTransactionAge: FiniteDuration, blacklistSrcAddresses: Set[String], allowBlacklistedTransferTo: Set[String])
