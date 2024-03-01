package com.wavesplatform.api.grpc

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.extensions.{Extension, Context as ExtensionContext}
import com.wavesplatform.settings.GRPCSettings
import com.wavesplatform.utils.ScorexLogging
import io.grpc.Server
import io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService
import monix.execution.Scheduler
import net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*

import java.net.InetSocketAddress
import java.util.concurrent.Executors
import scala.concurrent.Future

class GRPCServerExtension(context: ExtensionContext) extends Extension with ScorexLogging {
  private val settings = context.settings.config.as[GRPCSettings]("waves.grpc")
  private val executor = Executors.newFixedThreadPool(settings.workerThreads, new ThreadFactoryBuilder().setDaemon(true).setNameFormat("grpc-server-worker-%d").build())
  private implicit val apiScheduler: Scheduler = Scheduler(executor)
  private val bindAddress                      = new InetSocketAddress(settings.host, settings.port)
  private val server: Server = NettyServerBuilder
    .forAddress(bindAddress)
    .executor(executor)
    .addService(TransactionsApiGrpc.bindService(new TransactionsApiGrpcImpl(context.blockchain, context.transactionsApi), apiScheduler))
    .addService(BlocksApiGrpc.bindService(new BlocksApiGrpcImpl(context.blocksApi), apiScheduler))
    .addService(AccountsApiGrpc.bindService(new AccountsApiGrpcImpl(context.accountsApi), apiScheduler))
    .addService(AssetsApiGrpc.bindService(new AssetsApiGrpcImpl(context.assetsApi, context.accountsApi), apiScheduler))
    .addService(BlockchainApiGrpc.bindService(new BlockchainApiGrpcImpl(context.blockchain, context.settings.featuresSettings), apiScheduler))
    .addService(ProtoReflectionService.newInstance())
    .build()

  override def start(): Unit = {
    server.start()
    log.info(s"gRPC API was bound to $bindAddress")
  }

  override def shutdown(): Future[Unit] = {
    log.debug("Shutting down gRPC server")
    server.shutdown()
    Future(server.awaitTermination())(apiScheduler)
  }
}
