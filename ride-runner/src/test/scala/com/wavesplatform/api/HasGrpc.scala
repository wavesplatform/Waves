package com.wavesplatform.api

import cats.syntax.option.*
import com.google.common.primitives.Ints
import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.events.api.grpc.protobuf.BlockchainUpdatesApiGrpc.BlockchainUpdatesApi
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.{BlockchainUpdated, StateUpdate}
import com.wavesplatform.lang.API
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.block.Block
import com.wavesplatform.protobuf.transaction.DataTransactionData
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}
import io.grpc.inprocess.{InProcessChannelBuilder, InProcessServerBuilder}
import io.grpc.util.MutableHandlerRegistry
import io.grpc.{ManagedChannel, ServerServiceDefinition}

trait HasGrpc extends HasTestAccounts { this: BaseTestSuite =>
  def withGrpc[T](serviceImpl: BlockchainUpdatesApi)(test: ManagedChannel => T): T = {
    withGrpc(BlockchainUpdatesApi.bindService(serviceImpl, scala.concurrent.ExecutionContext.global))(test)
  }

  def withGrpc[T](serviceDef: ServerServiceDefinition)(f: ManagedChannel => T): T = {
    val serverName = InProcessServerBuilder.generateName()

    val serviceRegistry = new MutableHandlerRegistry()
    serviceRegistry.addService(serviceDef)

    val server  = InProcessServerBuilder.forName(serverName).fallbackHandlerRegistry(serviceRegistry).directExecutor().build().start()
    val channel = InProcessChannelBuilder.forName(serverName).directExecutor().build()
    try f(channel)
    finally {
      channel.shutdownNow()
      server.shutdown()
    }
  }

  def mkRollbackEvent(
      height: Int,
      forkNumber: Int,
      microBlockNumber: Int = 0,
      dataEntryUpdates: List[StateUpdate.DataEntryUpdate] = Nil
  ): SubscribeEvent = SubscribeEvent().withUpdate(
    BlockchainUpdated()
      .withId(toByteString32(forkNumber, height, microBlockNumber))
      .withHeight(height)
      .withUpdate(
        BlockchainUpdated.Update.Rollback(
          BlockchainUpdated
            .Rollback()
            .withRollbackStateUpdate(StateUpdate.defaultInstance.withDataEntries(dataEntryUpdates))
        )
      )
  )

  def mkMicroBlockAppendEvent(
      height: Int,
      forkNumber: Int,
      microBlockNumber: Int,
      dataEntryUpdates: List[StateUpdate.DataEntryUpdate] = Nil
  ): SubscribeEvent = SubscribeEvent().withUpdate(
    BlockchainUpdated()
      .withId(toByteString32(forkNumber, height, microBlockNumber))
      .withHeight(height)
      .withUpdate(
        BlockchainUpdated.Update.Append(
          BlockchainUpdated
            .Append()
            .withMicroBlock(BlockchainUpdated.Append.MicroBlockAppend())
            .withStateUpdate(StateUpdate.defaultInstance.withDataEntries(dataEntryUpdates))
        )
      )
  )

  def mkBlockAppendEvent(height: Int, forkNumber: Int, dataEntryUpdates: List[StateUpdate.DataEntryUpdate] = Nil): SubscribeEvent =
    SubscribeEvent().withUpdate(
      BlockchainUpdated()
        .withHeight(height)
        .withId(toByteString32(forkNumber, height))
        .withUpdate(
          BlockchainUpdated.Update.Append(
            BlockchainUpdated
              .Append()
              .withBlock(BlockchainUpdated.Append.BlockAppend().withBlock(mkPbBlock(height)))
              .withStateUpdate(StateUpdate.defaultInstance.withDataEntries(dataEntryUpdates))
          )
        )
    )

  def mkDataEntryUpdate(address: Address, key: String, valueBefore: Long, valueAfter: Long): StateUpdate.DataEntryUpdate =
    StateUpdate.DataEntryUpdate(
      address = DefaultBlockchainApi.toPb(address),
      dataEntry = DataTransactionData.DataEntry(key, DataTransactionData.DataEntry.Value.IntValue(valueAfter)).some,
      dataEntryBefore = DataTransactionData.DataEntry(key, DataTransactionData.DataEntry.Value.IntValue(valueBefore)).some
    )

  def mkPbBlock(height: Int) =
    Block.defaultInstance.withHeader(Block.Header.defaultInstance.withGenerator(toPb(miner.publicKey)).withTimestamp(height))

  def toPb(pk: PublicKey): ByteString = UnsafeByteOperations.unsafeWrap(pk.arr)

  def toByteString32(xs: Int*): ByteString = {
    require(xs.size < 4)
    UnsafeByteOperations.unsafeWrap(Array.concat(xs.map(Ints.toByteArray)*).padTo(32, 0.toByte))
  }

  def mkScript(scriptSrc: String): Script = {
    val estimator      = ScriptEstimatorV3(fixOverflow = true, overhead = false)
    val compiledScript = API.compile(input = scriptSrc, estimator).explicitGet()
    Script.fromBase64String(Base64.encode(compiledScript.bytes)).explicitGet()
  }
}
