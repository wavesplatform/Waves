package com.wavesplatform.events

import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.extensions.{Context, Extension}
import net.ceedubs.ficus.Ficus._
import com.wavesplatform.events.settings.BlockchainUpdatesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.utils.{ScorexLogging, forceStopApplication}

import scala.concurrent.Future

class BlockchainUpdates(private val context: Context) extends Extension with ScorexLogging with BlockchainUpdateTriggers {
  import monix.execution.Scheduler.Implicits.global

  private[this] val settings          = context.settings.config.as[BlockchainUpdatesSettings]("blockchain-updates")
  private[this] val repo: LevelDBRepo = ???

  override def start(): Unit = {
    log.info("BlockchainUpdates extension starting")
  }

  override def shutdown(): Future[Unit] = Future {
    log.info("BlockchainUpdates extension shitting down")
  }

  // todo stream events to already subscribed clients
  // for now, only updating database
  override def onProcessBlock(block: Block, diff: BlockDiffer.DetailedDiff, minerReward: Option[Long], blockchainBefore: Blockchain): Unit = {
    val newBlock                = BlockAppended.from(block, diff, minerReward, blockchainBefore)
    val (keyBlock, microBlocks) = repo.getLiquidState()
    val squashedBlock           = squash(keyBlock, microBlocks)
    repo.dropLiquidState()
    repo.appendBlock(squashedBlock)
    repo.appendBlock(newBlock)
  }

  override def onProcessMicroBlock(
      microBlock: MicroBlock,
      diff: BlockDiffer.DetailedDiff,
      blockchainBefore: Blockchain,
      totalBlockId: ByteStr
  ): Unit = {
    val newMicroBlock = MicroBlockAppended.from(microBlock, diff, blockchainBefore, totalBlockId)
    repo.appendMicroBlock(newMicroBlock)
  }

  override def onRollback(toBlockId: ByteStr, toHeight: Int): Unit = {
    repo.removeAfter(toHeight)
  }

  override def onMicroBlockRollback(toBlockId: ByteStr, height: Int): Unit = {
    repo.dropLiquidState(Some(toBlockId))
  }

  private def squash(keyBlock: BlockAppended, microBlocks: Seq[MicroBlockAppended]): BlockAppended = ???
}
