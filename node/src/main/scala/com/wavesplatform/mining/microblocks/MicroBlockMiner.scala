package com.wavesplatform.mining.microblocks

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.mining.{MinerDebugInfo, MiningConstraint}
import com.wavesplatform.settings.MinerSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import monix.eval.Task
import monix.execution.schedulers.SchedulerService
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
      settings: MinerSettings,
      minerScheduler: SchedulerService,
      appenderScheduler: SchedulerService,
      transactionAdded: Observable[Unit],
      nextMicroBlockSize: Int => Int = identity
  ): MicroBlockMiner =
    new MicroBlockMinerImpl(
      setDebugState,
      allChannels,
      blockchainUpdater,
      utx,
      settings,
      minerScheduler,
      appenderScheduler,
      transactionAdded,
      nextMicroBlockSize
    )
}
