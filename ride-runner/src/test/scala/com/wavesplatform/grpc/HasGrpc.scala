package com.wavesplatform.grpc

import com.wavesplatform.events.api.grpc.protobuf.BlockchainUpdatesApiGrpc.BlockchainUpdatesApi
import com.wavesplatform.resources.*
import io.grpc.inprocess.{InProcessChannelBuilder, InProcessServerBuilder}
import io.grpc.util.MutableHandlerRegistry
import io.grpc.{ManagedChannel, ServerServiceDefinition}

import scala.util.Using

trait HasGrpc {
  def withGrpc[T](serviceImpl: BlockchainUpdatesApi)(test: ManagedChannel => T): T = {
    withGrpc(BlockchainUpdatesApi.bindService(serviceImpl, scala.concurrent.ExecutionContext.global))(test)
  }

  def withGrpc[T](serviceDef: ServerServiceDefinition)(f: ManagedChannel => T): T = {
    val serverName = InProcessServerBuilder.generateName()

    val serviceRegistry = new MutableHandlerRegistry()
    serviceRegistry.addService(serviceDef)

    Using.resources(
      InProcessServerBuilder.forName(serverName).fallbackHandlerRegistry(serviceRegistry).directExecutor().build().start(),
      InProcessChannelBuilder.forName(serverName).directExecutor().build()
    )((_, channel) => f(channel))
  }
}
