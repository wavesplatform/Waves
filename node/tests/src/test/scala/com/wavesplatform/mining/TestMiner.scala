package com.wavesplatform.mining

import com.wavesplatform.account.KeyPair
import com.wavesplatform.state.Blockchain

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object TestMiner {
  object SafelyDisabled extends Miner with MinerDebugInfo {
    override def scheduleMining(baseBlockchain: Option[Blockchain], cancelMicroBlockMining: Boolean): Unit = {}
    override def getNextBlockGenerationOffset(account: KeyPair): Either[String, FiniteDuration] = Right(FiniteDuration(0, TimeUnit.SECONDS))
    override def state: MinerDebugInfo.State                                                    = MinerDebugInfo.Disabled
  }
}
