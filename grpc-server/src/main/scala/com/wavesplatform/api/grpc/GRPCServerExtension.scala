package com.wavesplatform.api.grpc

import java.net.InetSocketAddress

import com.wavesplatform.extensions.{Extension, Context => ExtensionContext}
import com.wavesplatform.settings.GRPCSettings
import com.wavesplatform.utils.ScorexLogging
import io.grpc.Server
import io.grpc.netty.NettyServerBuilder
import monix.execution.Scheduler
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.Future

class GRPCServerExtension extends Extension with ScorexLogging {
  @volatile
  var server: Server = _
  var context: ExtensionContext = _

  override def start(context: ExtensionContext): Unit = {
    this.context = context
    this.server = startServer(context)
  }

  override def shutdown(): Future[Unit] = {
    log.debug("Shutting down gRPC server")
    if (server != null && context != null) {
      server.shutdown()
      Future(server.awaitTermination())(context.actorSystem.dispatcher)
    } else {
      Future.successful(())
    }
  }

  private[this] def startServer(context: ExtensionContext): Server = {
    val settings = context.settings.config.as[GRPCSettings]("waves.grpc")
    implicit val apiScheduler: Scheduler = Scheduler(context.actorSystem.dispatcher)

    val bindAddress = new InetSocketAddress(settings.host, settings.port)
    val server: Server = NettyServerBuilder
      .forAddress(bindAddress)
      .addService(TransactionsApiGrpc.bindService(new TransactionsApiGrpcImpl(context.transactionsApi), apiScheduler))
      .addService(BlocksApiGrpc.bindService(new BlocksApiGrpcImpl(context.blocksApi), apiScheduler))
      .addService(AccountsApiGrpc.bindService(new AccountsApiGrpcImpl(context.accountsApi), apiScheduler))
      .addService(AssetsApiGrpc.bindService(new AssetsApiGrpcImpl(context.assetsApi, context.accountsApi), apiScheduler))
      .addService(BlockchainApiGrpc.bindService(new BlockchainApiGrpcImpl(context.blockchain, context.settings.featuresSettings), apiScheduler))
      .build()
      .start()

    log.info(s"gRPC API was bound to $bindAddress")
    server
  }
}
