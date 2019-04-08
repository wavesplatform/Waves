package com.wavesplatform.settings
import scala.concurrent.duration.FiniteDuration

case class DBSettings(directory: String,
                      storeTransactionsByAddress: Boolean,
                      maxCacheSize: Int,
                      maxRollbackDepth: Int,
                      rememberBlocks: FiniteDuration)
