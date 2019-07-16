package com.wavesplatform.settings
import scala.concurrent.duration.FiniteDuration

case class DBSettings(directory: String,
                      storeMinersBalanceInfo: Boolean,
                      storeTransactionsByAddress: Boolean,
                      storeInvokeScriptResults: Boolean,
                      maxCacheSize: Int,
                      maxRollbackDepth: Int,
                      rememberBlocks: FiniteDuration)
