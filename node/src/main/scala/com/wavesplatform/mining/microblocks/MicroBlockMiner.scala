package com.wavesplatform.mining.microblocks

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.mining.{MinerDebugInfo, MiningConstraint}
import com.wavesplatform.settings.MinerSettings
import com.wavesplatform.state.{Blockchain, EndorsementStorage}
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

trait MicroBlockMiner {
  def generateMicroBlockSequence(
      account: KeyPair,
      accumulatedBlock: Block,
      restTotalConstraint: MiningConstraint,
      lastMicroBlock: Long
  ): Task[Unit]
}

object MicroBlockMiner {
  def apply(
      setDebugState: MinerDebugInfo.State => Unit,
      allChannels: ChannelGroup,
      blockchainUpdater: BlockchainUpdater & Blockchain,
      utx: UtxPool,
      endorsementStorage: EndorsementStorage,
      settings: MinerSettings,
      minerScheduler: Scheduler,
      appenderScheduler: Scheduler,
      transactionAdded: Observable[Unit]
  ): MicroBlockMiner =
    new MicroBlockMinerImpl(
      setDebugState,
      allChannels,
      blockchainUpdater,
      utx,
      endorsementStorage,
      settings,
      minerScheduler,
      appenderScheduler,
      transactionAdded
    )
}
