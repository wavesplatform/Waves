package com.wavesplatform.events

import java.net.InetSocketAddress

import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.grpc.BlockchainUpdatesApiGrpcImpl
import com.wavesplatform.events.grpc.protobuf.BlockchainUpdatesApiGrpc
import com.wavesplatform.events.http.HttpServer
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

class BlockchainUpdates(private val context: Context) extends Extension with ScorexLogging with BlockchainUpdateTriggers {
  implicit val scheduler: Scheduler = Scheduler(context.actorSystem.dispatcher)

  private[this] val settings = context.settings.config.as[BlockchainUpdatesSettings]("blockchain-updates")

  private[this] val repo = new UpdatesRepoImpl(settings.directory)

  private[this] var grpcServer: Server     = null
  private[this] var httpServer: HttpServer = null

  override def start(): Unit = {
    log.info("BlockchainUpdates extension starting")

    // starting gRPC API
    val bindAddress = new InetSocketAddress("0.0.0.0", settings.grpcPort)

    grpcServer = NettyServerBuilder
      .forAddress(bindAddress)
      .addService(BlockchainUpdatesApiGrpc.bindService(new BlockchainUpdatesApiGrpcImpl(repo)(scheduler), scheduler))
      .build()
      .start()

    log.info(s"BlockchainUpdates extension started gRPC API on port ${settings.grpcPort}")

    // starting HTTP API
    httpServer = new HttpServer(settings.httpPort, repo)(context.actorSystem)
    httpServer.start()

    log.info(s"BlockchainUpdates extension started HTTP API on port ${settings.httpPort}")
  }

  // todo proper shutdown
  override def shutdown(): Future[Unit] = Future {
    log.info(s"BlockchainUpdates extension shutting down, last persisted height ${repo.height - 1}")

    if (httpServer != null) {
      httpServer.shutdown()
    }

    if (grpcServer != null) {
      grpcServer.shutdown()
      grpcServer.awaitTermination()
    }

    repo.shutdown()
  }

  override def onProcessBlock(block: Block, diff: BlockDiffer.DetailedDiff, minerReward: Option[Long], blockchainBefore: Blockchain): Unit = {
    val newBlock = BlockAppended.from(block, diff, minerReward, blockchainBefore)
    repo.appendBlock(newBlock)
    // todo log.debug, and make it every 100 blocks, like in BlockchainUpdater
    if (newBlock.toHeight % 100 == 0) {
      log.info(s"BlockchainUpdates extension appended blocks up to ${newBlock.toHeight}")
//      Thread.sleep(5000)
    }
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
    val rollbackCompleted = RollbackCompleted(toBlockId, toHeight)
    repo.rollback(rollbackCompleted)
  }

  override def onMicroBlockRollback(toBlockId: ByteStr, height: Int): Unit = {
    val microBlockRollbackCompleted = MicroBlockRollbackCompleted(toBlockId, height)
    repo.rollbackMicroBlock(microBlockRollbackCompleted)
  }
}
