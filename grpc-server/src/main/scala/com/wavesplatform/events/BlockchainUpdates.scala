package com.wavesplatform.events

import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.RDB
import com.wavesplatform.events.api.grpc.protobuf.BlockchainUpdatesApiGrpc
import com.wavesplatform.events.settings.BlockchainUpdatesSettings
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.state.{Blockchain, StateSnapshot}
import com.wavesplatform.utils.{Schedulers, ScorexLogging}
import io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService
import io.grpc.{Metadata, Server, ServerStreamTracer, Status}
import monix.execution.schedulers.SchedulerService
import monix.execution.{ExecutionModel, Scheduler, UncaughtExceptionReporter}
import net.ceedubs.ficus.Ficus.*

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.util.Try

class BlockchainUpdates(private val context: Context) extends Extension with ScorexLogging with BlockchainUpdateTriggers {
  private[this] implicit val scheduler: SchedulerService = Schedulers.fixedPool(
    sys.runtime.availableProcessors(),
    "blockchain-updates",
    UncaughtExceptionReporter(err => log.error("Uncaught exception in BlockchainUpdates scheduler", err)),
    ExecutionModel.Default,
    rejectedExecutionHandler = new akka.dispatch.SaneRejectedExecutionHandler
  )

  private[this] val settings = context.settings.config.as[BlockchainUpdatesSettings]("waves.blockchain-updates")
  // todo: no need to open column families here
  private[this] val rdb  = RDB.open(context.settings.dbSettings.copy(directory = context.settings.directory + "/blockchain-updates"))
  private[this] val repo = new Repo(rdb.db, context.blocksApi)

  private[this] val grpcServer: Server = NettyServerBuilder
    .forAddress(new InetSocketAddress("0.0.0.0", settings.grpcPort))
    .permitKeepAliveTime(settings.minKeepAlive.toNanos, TimeUnit.NANOSECONDS)
    .addStreamTracerFactory((fullMethodName: String, headers: Metadata) =>
      new ServerStreamTracer {
        private[this] var callInfo = Option.empty[ServerStreamTracer.ServerCallInfo[?, ?]]
        private[this] def callId   = callInfo.fold("???")(ci => Integer.toHexString(System.identityHashCode(ci)))

        override def serverCallStarted(callInfo: ServerStreamTracer.ServerCallInfo[?, ?]): Unit = {
          this.callInfo = Some(callInfo)
          log.trace(s"[$callId] gRPC call started: $fullMethodName, headers: $headers")
        }

        override def streamClosed(status: Status): Unit =
          log.trace(s"[$callId] gRPC call closed with status: $status")
      }
    )
    .addService(BlockchainUpdatesApiGrpc.bindService(repo, scheduler))
    .addService(ProtoReflectionService.newInstance())
    .build()

  override def start(): Unit = {
    log.info(s"BlockchainUpdates extension starting with settings $settings")

    val nodeHeight      = context.blockchain.height
    val extensionHeight = repo.height

    if (extensionHeight < nodeHeight)
      throw new IllegalStateException(s"BlockchainUpdates height $extensionHeight is lower than node height $nodeHeight")

    if (extensionHeight > nodeHeight) {
      log.info(s"Rolling back from $extensionHeight to node height $nodeHeight")
      repo.rollbackData(nodeHeight)
    }

    val lastUpdateId = Try(ByteStr(repo.getBlockUpdate(nodeHeight).getUpdate.id.toByteArray)).toOption
    val lastBlockId  = context.blockchain.blockHeader(nodeHeight).map(_.id())

    if (lastUpdateId != lastBlockId)
      throw new IllegalStateException(s"Last update ID $lastUpdateId does not match last block ID $lastBlockId at height $nodeHeight")

    log.info(s"BlockchainUpdates startup check successful at height $nodeHeight")

    grpcServer.start()
    log.info(s"BlockchainUpdates extension started gRPC API on port ${settings.grpcPort}")
  }

  override def shutdown(): Future[Unit] =
    Future {
      grpcServer.shutdown()
      grpcServer.awaitTermination(10, TimeUnit.SECONDS)

      scheduler.shutdown()
      scheduler.awaitTermination(10 seconds)
      repo.shutdown()
      rdb.close()
    }(Scheduler.global)

  override def onProcessBlock(
      block: Block,
      snapshot: StateSnapshot,
      reward: Option[Long],
      hitSource: ByteStr,
      blockchainBeforeWithReward: Blockchain
  ): Unit = repo.onProcessBlock(block, snapshot, reward, hitSource, blockchainBeforeWithReward)

  override def onProcessMicroBlock(
      microBlock: MicroBlock,
      snapshot: StateSnapshot,
      blockchainBeforeWithReward: Blockchain,
      totalBlockId: ByteStr,
      totalTransactionsRoot: ByteStr
  ): Unit = repo.onProcessMicroBlock(microBlock, snapshot, blockchainBeforeWithReward, totalBlockId, totalTransactionsRoot)

  override def onRollback(blockchainBefore: Blockchain, toBlockId: ByteStr, toHeight: Int): Unit =
    repo.onRollback(blockchainBefore, toBlockId, toHeight)

  override def onMicroBlockRollback(blockchainBefore: Blockchain, toBlockId: ByteStr): Unit =
    repo.onMicroBlockRollback(blockchainBefore, toBlockId)
}
