package com.wavesplatform.api

import cats.syntax.option.*
import com.wavesplatform.events.api.grpc.protobuf.BlockchainUpdatesApiGrpc.BlockchainUpdatesApi
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.block.{Block, MicroBlock}
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}
import io.grpc.inprocess.{InProcessChannelBuilder, InProcessServerBuilder}
import io.grpc.util.MutableHandlerRegistry
import io.grpc.{ManagedChannel, ServerServiceDefinition}

trait HasGrpc extends CommonGrpcConverters with HasTestAccounts { this: BaseTestSuite =>
  def withGrpc[T](serviceImpl: BlockchainUpdatesApi)(test: ManagedChannel => T): T = {
    withGrpc(BlockchainUpdatesApi.bindService(serviceImpl, scala.concurrent.ExecutionContext.global))(test)
  }

  def withGrpc[T](serviceDefs: ServerServiceDefinition*)(f: ManagedChannel => T): T = {
    val serverName = InProcessServerBuilder.generateName()

    val serviceRegistry = new MutableHandlerRegistry()
    serviceDefs.foreach(serviceRegistry.addService)

    val server  = InProcessServerBuilder.forName(serverName).fallbackHandlerRegistry(serviceRegistry).directExecutor().build().start()
    val channel = InProcessChannelBuilder.forName(serverName).directExecutor().build()
    try f(channel)
    finally {
      channel.shutdownNow()
      server.shutdown()
    }
  }

  def mkPbBlock(height: Int) = Block(header =
    Block
      .Header(
        generator = miner.publicKey.toByteString,
        timestamp = height
      )
      .some
  )

  def mkPbMicroBlock = MicroBlock(senderPublicKey = miner.publicKey.toByteString)
}
