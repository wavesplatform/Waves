package com.wavesplatform.events

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.api.grpc.BlockchainUpdatesApiGrpcImpl
import com.wavesplatform.events.api.grpc.protobuf.BlockchainUpdatesApiGrpc
import com.wavesplatform.events.repo.UpdatesRepoImpl
import com.wavesplatform.events.settings.BlockchainUpdatesSettings
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.utils.{Schedulers, ScorexLogging}
import io.grpc.{Metadata, Server, ServerStreamTracer, Status}
import io.grpc.netty.NettyServerBuilder
import monix.execution.Scheduler
import net.ceedubs.ficus.Ficus._

class BlockchainUpdates(private val context: Context) extends Extension with ScorexLogging with BlockchainUpdateTriggers {
  private[this] implicit val scheduler = Schedulers.fixedPool(sys.runtime.availableProcessors(), "blockchain-updates")

  private[this] val settings = context.settings.config.as[BlockchainUpdatesSettings]("waves.blockchain-updates")
  private[this] val repo     = new UpdatesRepoImpl(s"${context.settings.directory}/blockchain-updates", context.blocksApi)

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
    } else if (nodeHeight == 0) {
      if (extensionHeight > 0) log.warn("Data has been reset, dropping entire blockchain updates data")
      repo.rollback(context.blockchain, ByteStr.empty, 0, sendEvent = false).get
    } else {
      (repo.updateForHeight(nodeHeight), context.blockchain.blockHeader(nodeHeight)) match {
        case (Success(extensionBlockAtNodeHeight), Some(lastNodeBlockHeader)) =>
          val lastNodeBlockId = lastNodeBlockHeader.id()

          // check if extension is on fork. Block ids must be equal at node height
          if (extensionBlockAtNodeHeight.id != lastNodeBlockId) {
            val exception = new IllegalStateException(
              s"BlockchainUpdates extension has forked: at node height $nodeHeight node block id is $lastNodeBlockId, extension's is ${extensionBlockAtNodeHeight.id}"
            )
            log.error("BlockchainUpdates startup check failed", exception)
            throw exception
          }

          // if not on fork, but extension moved higher than node, rollback the extension to recover
          if (extensionHeight > nodeHeight) {
            log.warn(s"BlockchainUpdates at height $extensionHeight is higher than node at height $nodeHeight, rolling back BlockchainUpdates")
            repo
              .rollback(context.blockchain, extensionBlockAtNodeHeight.id, extensionBlockAtNodeHeight.height, sendEvent = false)
              .recoverWith { case err: Throwable => Failure(new RuntimeException("BlockchainUpdates failed to rollback at startup", err)) }
              .get
          }
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
      .permitKeepAliveTime(settings.minKeepAlive.toNanos, TimeUnit.NANOSECONDS)
      .addStreamTracerFactory(
        (fullMethodName: String, headers: Metadata) =>
          new ServerStreamTracer {
            var callInfo             = Option.empty[ServerStreamTracer.ServerCallInfo[_, _]]
            private[this] def callId = callInfo.fold("???")(ci => Integer.toHexString(System.identityHashCode(ci)))

            override def serverCallStarted(callInfo: ServerStreamTracer.ServerCallInfo[_, _]): Unit = {
              this.callInfo = Some(callInfo)
              log.trace(s"[$callId] gRPC call started: $fullMethodName, headers: $headers")
            }

            override def streamClosed(status: Status): Unit = {
              log.trace(s"[$callId] gRPC call closed with status: $status")
            }
          }
      )
      .addService(BlockchainUpdatesApiGrpc.bindService(new BlockchainUpdatesApiGrpcImpl(repo)(scheduler), scheduler))
      .build()
      .start()

    log.info(s"BlockchainUpdates extension started gRPC API on port ${settings.grpcPort}")
  }

  override def shutdown(): Future[Unit] =
    Future {
      log.info(s"BlockchainUpdates extension shutting down, last persisted height ${repo.height.get - 1}")

      if (grpcServer != null) {
        grpcServer.shutdown()
        grpcServer.awaitTermination(10, TimeUnit.SECONDS)
        grpcServer = null
      }

      scheduler.shutdown()
      scheduler.awaitTermination(10 seconds)
      repo.shutdown()
    }(Scheduler.global)

  override def onProcessBlock(
      block: Block,
      diff: BlockDiffer.DetailedDiff,
      minerReward: Option[Long],
      blockchainBeforeWithMinerReward: Blockchain
  ): Unit = {
    val newBlock = BlockAppended.from(block, diff, blockchainBeforeWithMinerReward)
    repo.appendBlock(newBlock).get
    if (newBlock.height % 100 == 0) {
      log.debug(s"BlockchainUpdates appended blocks up to ${newBlock.height}")
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

  override def onRollback(blockchainBefore: Blockchain, toBlockId: ByteStr, toHeight: Int): Unit = {
    repo.rollback(blockchainBefore, toBlockId, toHeight).get
  }

  override def onMicroBlockRollback(blockchainBefore: Blockchain, toBlockId: ByteStr): Unit = {
    repo.rollbackMicroBlock(blockchainBefore, toBlockId).get
  }
}
