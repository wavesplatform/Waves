package com.wavesplatform.events

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.api.grpc.BlockchainUpdatesApiGrpcImpl
import com.wavesplatform.events.api.grpc.protobuf.BlockchainUpdatesApiGrpc
import com.wavesplatform.events.repo.UpdatesRepoImpl
import com.wavesplatform.events.settings.BlockchainUpdatesSettings
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.utils.ScorexLogging
import io.grpc.Server
import io.grpc.netty.NettyServerBuilder
import monix.execution.Scheduler
import net.ceedubs.ficus.Ficus._

import scala.concurrent.Future
import scala.util.{Failure, Success}

class BlockchainUpdates(private val context: Context) extends Extension with ScorexLogging with BlockchainUpdateTriggers {
  implicit val scheduler: Scheduler = Scheduler(context.actorSystem.dispatcher)

  private[this] val settings = context.settings.config.as[BlockchainUpdatesSettings]("blockchain-updates")

  private[this] val repo = new UpdatesRepoImpl(s"${context.settings.directory}/blockchain-updates")

  private[this] var grpcServer: Server = null

  override def start(): Unit = {
    log.info(s"BlockchainUpdates extension starting with settings $settings")

    // startup checks
    val nodeHeight      = context.blockchain.height
    val extensionHeight = repo.height.get
    if (extensionHeight < nodeHeight) {
      val exception = new IllegalStateException(s"BlockchainUpdates at height $extensionHeight is lower than node at height $nodeHeight")
      log.error("BlockchainUpdates startup check failed", exception)
      throw exception
    } else if (nodeHeight > 0) {
      (repo.updateForHeight(nodeHeight), context.blockchain.blockHeader(nodeHeight)) match {
        case (Success(Some(extensionBlockAtNodeHeight)), Some(lastNodeBlockHeader)) =>
          val lastNodeBlockId = lastNodeBlockHeader.id.value()

          // check if extension is on fork. Block ids must be equal at node height
          if (extensionBlockAtNodeHeight.toId != lastNodeBlockId) {
            val exception = new IllegalStateException(
              s"BlockchainUpdates extension has forked: at node height $nodeHeight node block id is $lastNodeBlockId, extension's is ${extensionBlockAtNodeHeight.toId}"
            )
            log.error("BlockchainUpdates startup check failed", exception)
            throw exception
          }

          // if not on fork, but extension moved higher than node, rollback the extension to recover
          if (extensionHeight > nodeHeight) {
            log.warn(s"BlockchainUpdates at height $extensionHeight is higher than node at height $nodeHeight, rolling back BlockchainUpdates")
            repo
              .rollback(RollbackCompleted(extensionBlockAtNodeHeight.toId, extensionBlockAtNodeHeight.toHeight))
              .recoverWith { case _: Throwable => Failure(new RuntimeException("BlockchainUpdates failed to rollback at startup")) }
              .get
          }
        case (Success(None), Some(_)) =>
          val exception = new RuntimeException(
            s"BlockchainUpdates has no block at height $nodeHeight, while node has one at startup. Extension height: $extensionHeight, node height: $nodeHeight"
          )
          log.error("BlockchainUpdates startup check failed", exception)
          throw exception
        case (Failure(ex), _) =>
          val exception = new RuntimeException(s"BlockchainUpdates failed to get extension block info at node height at startup", ex)
          log.error("BlockchainUpdates startup check failed", ex)
          throw exception
        case (Success(_), None) =>
          val exception = new RuntimeException(s"Incorrect node state: missing block at height $nodeHeight")
          log.error("BlockchainUpdates startup check failed", exception)
          throw exception
        case _ =>
          val exception = new RuntimeException(
            s"BlockchainUpdates failed to perform a startup check. Extension height: $extensionHeight, node height: $nodeHeight"
          )
          log.error("BlockchainUpdates startup check failed", exception)
          throw exception
      }
    }
    log.info(s"BlockchainUpdates startup check successful at height $extensionHeight")

    // starting gRPC API
    val bindAddress = new InetSocketAddress("0.0.0.0", settings.grpcPort)

    grpcServer = NettyServerBuilder
      .forAddress(bindAddress)
      .addService(BlockchainUpdatesApiGrpc.bindService(new BlockchainUpdatesApiGrpcImpl(repo)(scheduler), scheduler))
      .build()
      .start()

    log.info(s"BlockchainUpdates extension started gRPC API on port ${settings.grpcPort}")
  }

  override def shutdown(): Future[Unit] = Future {
    log.info(s"BlockchainUpdates extension shutting down, last persisted height ${repo.height.get - 1}")

    if (grpcServer != null) {
      grpcServer.shutdown()
      grpcServer.awaitTermination(10L, TimeUnit.SECONDS)
    }

    repo.shutdown()
  }

  override def onProcessBlock(
      block: Block,
      diff: BlockDiffer.DetailedDiff,
      minerReward: Option[Long],
      blockchainBeforeWithMinerReward: Blockchain
  ): Unit = {
    val newBlock = BlockAppended.from(block, diff, minerReward, blockchainBeforeWithMinerReward)
    repo.appendBlock(newBlock).get
    if (newBlock.toHeight % 100 == 0) {
      log.debug(s"BlockchainUpdates appended blocks up to ${newBlock.toHeight}")
    }
  }

  override def onProcessMicroBlock(
      microBlock: MicroBlock,
      diff: BlockDiffer.DetailedDiff,
      blockchainBeforeWithMinerReward: Blockchain,
      totalBlockId: ByteStr,
      totalTransactionsRoot: ByteStr
  ): Unit = {
    val newMicroBlock = MicroBlockAppended.from(microBlock, diff, blockchainBeforeWithMinerReward, totalBlockId, totalTransactionsRoot)
    repo.appendMicroBlock(newMicroBlock).get
  }

  override def onRollback(toBlockId: ByteStr, toHeight: Int): Unit = {
    val rollbackCompleted = RollbackCompleted(toBlockId, toHeight)
    repo.rollback(rollbackCompleted).get
  }

  override def onMicroBlockRollback(toBlockId: ByteStr, height: Int): Unit = {
    val microBlockRollbackCompleted = MicroBlockRollbackCompleted(toBlockId, height)
    repo.rollbackMicroBlock(microBlockRollbackCompleted).get
  }
}
