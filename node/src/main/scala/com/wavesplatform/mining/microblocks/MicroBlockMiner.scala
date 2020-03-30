package com.wavesplatform.mining.microblocks

import cats.effect.concurrent.Ref
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.{MinerDebugInfo, MiningConstraint, MiningConstraints}
import com.wavesplatform.settings.MinerSettings
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._

trait MicroBlockMiner {
  def generateMicroBlockSequence(
      account: KeyPair,
      accumulatedBlock: Block,
      delay: FiniteDuration,
      constraints: MiningConstraints,
      restTotalConstraint: MiningConstraint
  ): Task[Unit]
}

object MicroBlockMiner {
  def apply(
      debugState: Ref[Task, MinerDebugInfo.State],
      allChannels: ChannelGroup,
      appendMicroBlockTask: MicroBlock => Task[Either[ValidationError, BlockId]],
      utx: UtxPool,
      settings: MinerSettings,
      minerScheduler: Scheduler
  ): MicroBlockMiner =
    new MicroBlockMinerImpl(
      debugState,
      allChannels,
      appendMicroBlockTask,
      utx,
      settings,
      minerScheduler
    )
}
