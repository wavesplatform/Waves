package com.wavesplatform.settings

import com.wavesplatform.account.PrivateKey
import com.wavesplatform.mining.Miner
import pureconfig.*
import pureconfig.generic.derivation.default.*

import scala.concurrent.duration.FiniteDuration

case class MinerSettings(
    enable: Boolean,
    quorum: Int,
    intervalAfterLastBlockThenGenerationIsAllowed: FiniteDuration,
    noQuorumMiningDelay: FiniteDuration,
    microBlockInterval: FiniteDuration,
    minimalBlockGenerationOffset: FiniteDuration,
    maxTransactionsInMicroBlock: Int,
    minMicroBlockAge: FiniteDuration,
    privateKeys: Seq[PrivateKey]
) derives ConfigReader {
  require(maxTransactionsInMicroBlock <= Miner.MaxTransactionsPerMicroblock)
}
