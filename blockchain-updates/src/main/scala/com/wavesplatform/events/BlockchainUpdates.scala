package com.wavesplatform.events

import java.net.InetSocketAddress

import cats.syntax.monoid._
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.repo.UpdatesRepoImpl
import com.wavesplatform.events.grpc.BlockchainUpdatesApiGrpcImpl
import com.wavesplatform.events.grpc.protobuf.BlockchainUpdatesApiGrpc
import com.wavesplatform.events.http.HttpServer
import com.wavesplatform.extensions.{Context, Extension}
import net.ceedubs.ficus.Ficus._
import com.wavesplatform.events.settings.BlockchainUpdatesSettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.utils.ScorexLogging
import io.grpc.Server
import io.grpc.netty.NettyServerBuilder
import monix.execution.Scheduler
import com.wavesplatform.database.openDB
import monix.reactive.subjects.{ConcurrentSubject, Subject}

import scala.concurrent.Future

class BlockchainUpdates(private val context: Context) extends Extension with ScorexLogging with BlockchainUpdateTriggers {
  implicit val scheduler: Scheduler = Scheduler(context.actorSystem.dispatcher)

  private[this] val settings = context.settings.config.as[BlockchainUpdatesSettings]("blockchain-updates")

  private[this] val db = openDB(settings.directory)
  log.info(s"BlockchainUpdates extension opened db at ${settings.directory}")
  private[this] val repo = new UpdatesRepoImpl(db)

  private[this] var grpcServer: Server     = null
  private[this] var httpServer: HttpServer = null

  private[this] val currentUpdates = ConcurrentSubject.publish[BlockchainUpdated]

  override def start(): Unit = {
    log.info("BlockchainUpdates extension starting")

    // ensure there is no liquid state remaining (from previous restart/crash, etc.)
    repo.dropLiquidState()

    // starting gRPC API
    val bindAddress = new InetSocketAddress("0.0.0.0", settings.grpcPort)

    grpcServer = NettyServerBuilder
      .forAddress(bindAddress)
      .addService(BlockchainUpdatesApiGrpc.bindService(new BlockchainUpdatesApiGrpcImpl(repo, currentUpdates)(scheduler), scheduler))
      .build()
      .start()

    log.info(s"BlockchainUpdates extension started gRPC API on port ${settings.grpcPort}")

    // starting HTTP API
    httpServer = new HttpServer(settings.httpPort, repo)(context.actorSystem)
    httpServer.start()

    log.info(s"BlockchainUpdates extension started HTTP API on port ${settings.httpPort}")
  }

  override def shutdown(): Future[Unit] = Future {
    log.info("BlockchainUpdates extension shutting down")

    // node does not persist liquid state, neither should the extension
    if (repo != null) {
      repo.dropLiquidState()
    }

    if (httpServer != null) {
      httpServer.shutdown()
    }

    if (grpcServer != null) {
      grpcServer.shutdown()
      grpcServer.awaitTermination()
    }

    db.close()
  }

  // todo stream events to already subscribed clients
  // for now, only updating database
  override def onProcessBlock(block: Block, diff: BlockDiffer.DetailedDiff, minerReward: Option[Long], blockchainBefore: Blockchain): Unit = {
    val newBlock = BlockAppended.from(block, diff, minerReward, blockchainBefore)
    repo.appendBlock(newBlock)
    currentUpdates.onNext(newBlock)
  }

  override def onProcessMicroBlock(
      microBlock: MicroBlock,
      diff: BlockDiffer.DetailedDiff,
      blockchainBefore: Blockchain,
      totalBlockId: ByteStr
  ): Unit = {
    val newMicroBlock = MicroBlockAppended.from(microBlock, diff, blockchainBefore, totalBlockId)
    repo.appendMicroBlock(newMicroBlock)
    currentUpdates.onNext(newMicroBlock)
  }

  override def onRollback(toBlockId: ByteStr, toHeight: Int): Unit = {
    repo.removeAfter(toHeight)
    currentUpdates.onNext(RollbackCompleted(toBlockId, toHeight))
  }

  override def onMicroBlockRollback(toBlockId: ByteStr, height: Int): Unit = {
    repo.dropLiquidState(Some(toBlockId))
    currentUpdates.onNext(MicroBlockRollbackCompleted(toBlockId, height))
  }
}
