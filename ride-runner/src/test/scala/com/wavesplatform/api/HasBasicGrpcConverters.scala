package com.wavesplatform.api

import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.{BlockchainUpdated, StateUpdate}
import com.wavesplatform.protobuf.block.Block

trait HasBasicGrpcConverters { this: HasGrpc =>
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

  def mkBlockAppendEvent(height: Int, forkNumber: Int, dataEntryUpdates: List[StateUpdate.DataEntryUpdate]): SubscribeEvent =
    mkBlockAppendEvent(height, forkNumber, _.withStateUpdate(StateUpdate.defaultInstance.withDataEntries(dataEntryUpdates)))

  def mkBlockAppendEvent(
      height: Int,
      forkNumber: Int,
      modAppend: BlockchainUpdated.Append => BlockchainUpdated.Append = identity,
      modBlock: Block => Block = identity
  ): SubscribeEvent =
    SubscribeEvent().withUpdate(
      BlockchainUpdated()
        .withId(toByteString32(forkNumber, height))
        .withHeight(height)
        .withUpdate(
          BlockchainUpdated.Update.Append(
            modAppend(
              BlockchainUpdated
                .Append()
                .withBlock(BlockchainUpdated.Append.BlockAppend().withBlock(modBlock(mkPbBlock(height))))
            )
          )
        )
    )
}
