package com.wavesplatform.api.grpc

import java.net.InetSocketAddress
import scala.concurrent.Future
import com.wavesplatform.extensions.{Extension, Context as ExtensionContext}
import com.wavesplatform.settings.GRPCSettings
import com.wavesplatform.utils.ScorexLogging
import io.grpc.Server
import io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService
import monix.execution.Scheduler
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*

class GRPCServerExtension(context: ExtensionContext) extends Extension with ScorexLogging {
  private implicit val apiScheduler: Scheduler = Scheduler(context.actorSystem.dispatcher)
  private val settings                         = context.settings.config.as[GRPCSettings]("waves.grpc")
  private val bindAddress                      = new InetSocketAddress(settings.host, settings.port)
  private val server: Server = NettyServerBuilder
    .forAddress(bindAddress)
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
    Future(server.awaitTermination())(context.actorSystem.dispatcher)
  }
}
