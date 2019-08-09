package com.wavesplatform.mining.microblocks

import cats.effect.concurrent.Ref
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.mining.{MinerDebugInfo, MiningConstraint, MiningConstraints}
import com.wavesplatform.settings.MinerSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import monix.eval.Task
import monix.execution.schedulers.SchedulerService

import scala.concurrent.duration._

trait MicroBlockMiner {
  def generateMicroBlockSequence(account: KeyPair,
                                 accumulatedBlock: Block,
                                 delay: FiniteDuration,
                                 constraints: MiningConstraints,
                                 restTotalConstraint: MiningConstraint): Task[Unit]
}

object MicroBlockMiner {
  def apply(debugState: Ref[Task, MinerDebugInfo.State],
            allChannels: ChannelGroup,
            blockchainUpdater: BlockchainUpdater with Blockchain,
            utx: UtxPool,
            settings: MinerSettings,
            minerScheduler: SchedulerService,
            appenderScheduler: SchedulerService): MicroBlockMiner =
    new MicroBlockMinerImpl(
      debugState,
      allChannels,
      blockchainUpdater,
      utx,
      settings,
      minerScheduler,
      appenderScheduler,
    )
}
