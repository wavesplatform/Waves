package com.wavesplatform.ride.blockchain

import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.state.Height

sealed trait BlockchainState extends Product with Serializable
object BlockchainState {
  case class Working(height: Height) extends BlockchainState
  case class TransientRollback(origHeight: Height, rollbackHeight: Height, deferReverted: List[SubscribeEvent]) extends BlockchainState {
    def append(action: SubscribeEvent): TransientRollback = copy(deferReverted = action :: deferReverted)
    def append(action: SubscribeEvent, newRollbackHeight: Int): TransientRollback =
      copy(deferReverted = action :: deferReverted, rollbackHeight = Height(newRollbackHeight))
  }

  def apply(orig: BlockchainState, event: SubscribeEvent): (BlockchainState, Seq[SubscribeEvent]) = {
    val update = event.getUpdate.update
    val height = event.getUpdate.height
    orig match {
      case orig: Working =>
        update match {
          case _: Update.Append   => (orig, Seq(event))
          case _: Update.Rollback => (TransientRollback(orig.height, Height(height), List(event)), Nil)
          case Update.Empty       => (orig, Nil)
        }

      case orig: TransientRollback =>
        update match {
          case _: Update.Append =>
            // TODO, microblocks
            if (height >= orig.origHeight) (Working(Height(height)), orig.deferReverted.reverse)
            else (orig.append(event), Nil)

          case _: Update.Rollback => (orig.append(event, height), Nil)
          case Update.Empty       => (orig, Nil)
        }
    }
  }
}
