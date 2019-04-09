package com.wavesplatform.api.grpc

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.GRPCSettings
import com.wavesplatform.utils.ScorexLogging
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import io.grpc.{Server, ServerBuilder}
import monix.execution.Scheduler

class GRPCServerExtension(context: com.wavesplatform.extensions.Context) extends ScorexLogging {
  val settings = ConfigFactory.load().as[GRPCSettings]("waves.grpc")
  val server   = startServer(settings)

  private[this] def startServer(settings: GRPCSettings): Server = {
    implicit val apiScheduler = Scheduler(context.actorSystem.dispatcher)

    val server: Server = ServerBuilder
      .forPort(settings.port)
      .addService(TransactionsApiGrpc.bindService(
        new TransactionsApiGrpcImpl(context.settings.blockchainSettings.functionalitySettings,
                                    context.wallet,
                                    context.blockchain,
                                    context.utx,
                                    context.channels),
        apiScheduler
      ))
      .addService(BlocksApiGrpc.bindService(new BlocksApiGrpcImpl(context.blockchain), apiScheduler))
      .build()
      .start()

    log.info(s"gRPC API was bound to ${settings.port}")
    context.actorSystem.registerOnTermination {
      log.debug("Shutting down gRPC server")
      server.shutdown()
    }
    server
  }
}
