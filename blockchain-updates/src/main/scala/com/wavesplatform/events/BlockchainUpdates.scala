package com.wavesplatform.events

import java.net.InetSocketAddress

import cats.syntax.monoid._
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.grpc.BlockchainUpdatesApiGrpcImpl
import com.wavesplatform.events.grpc.protobuf.BlockchainUpdatesApiGrpc
import com.wavesplatform.extensions.{Context, Extension}
import net.ceedubs.ficus.Ficus._
import com.wavesplatform.events.settings.BlockchainUpdatesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.utils.{ScorexLogging, forceStopApplication}
import io.grpc.Server
import io.grpc.netty.NettyServerBuilder
import monix.execution.Scheduler

import scala.concurrent.Future

class BlockchainUpdates(private val context: Context) extends Extension with ScorexLogging with BlockchainUpdateTriggers {
  import monix.execution.Scheduler.Implicits.global

  private[this] val settings          = context.settings.config.as[BlockchainUpdatesSettings]("blockchain-updates")
  private[this] var repo: UpdatesRepo = ???
  private[this] var server: Server    = null

  override def start(): Unit = {
    log.info("BlockchainUpdates extension starting")

    // ensure there is no liquid state remaining (from previous restart/crash, etc.)
    repo.dropLiquidState()

    implicit val apiScheduler: Scheduler = Scheduler(context.actorSystem.dispatcher)

    val bindAddress = new InetSocketAddress("0.0.0.0", settings.port)

    server = NettyServerBuilder
      .forAddress(bindAddress)
      .addService(BlockchainUpdatesApiGrpc.bindService(new BlockchainUpdatesApiGrpcImpl(repo)(apiScheduler), apiScheduler))
      .build()
      .start()

    log.info(s"BlockchainUpdates extension started gRPC API on port ${settings.port}")
  }

  override def shutdown(): Future[Unit] = Future {
    log.info("BlockchainUpdates extension shutting down")

    // node does not persist liquid state, neither should the extension
    if (repo != null) {
      repo.dropLiquidState()
    }

    if (server != null) {
      server.shutdown()
      Future(server.awaitTermination())(context.actorSystem.dispatcher)
    } else {
      Future.successful(())
    }
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

  private def squash(keyBlock: BlockAppended, microBlocks: Seq[MicroBlockAppended]): BlockAppended = {
    val totalResBlockSig        = microBlocks.lastOption.fold(keyBlock.block.signature)(_.microBlock.totalResBlockSig)
    val transactionData         = microBlocks.foldLeft(keyBlock.block.transactionData)((txs, mb) => txs ++ mb.microBlock.transactionData)
    val blockStateUpdate        = microBlocks.foldLeft(keyBlock.blockStateUpdate)((upd, mb) => upd.combine(mb.microBlockStateUpdate))
    val transactionStateUpdates = microBlocks.foldLeft(keyBlock.transactionStateUpdates)((upds, mb) => upds ++ mb.transactionStateUpdates)

    // todo make sure generationSignature and transactionsRoot are correct in Block
    // not touching them for now
    BlockAppended(
      toId = totalResBlockSig,
      toHeight = keyBlock.toHeight,
      block = keyBlock.block.copy(
        signature = totalResBlockSig,
        transactionData = transactionData
      ),
      updatedWavesAmount = keyBlock.updatedWavesAmount,
      blockStateUpdate = blockStateUpdate,
      transactionStateUpdates = transactionStateUpdates
    )
  }
}
