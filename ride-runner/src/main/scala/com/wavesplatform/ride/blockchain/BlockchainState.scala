package com.wavesplatform.ride.blockchain

import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging

sealed trait BlockchainState extends Product with Serializable
object BlockchainState extends ScorexLogging {
  case class Working(height: Height) extends BlockchainState {
    def withHeight(height: Height): Working = copy(height = height)
    override def toString: String           = s"Working($height)"
  }
  case class TransientRollback(origHeight: Height, currHeight: Height, deferReverted: List[SubscribeEvent]) extends BlockchainState {
    def withAction(action: SubscribeEvent): TransientRollback =
      copy(deferReverted = action :: deferReverted, currHeight = Height(action.getUpdate.height))

    def withHeight(currHeight: Height): TransientRollback = copy(currHeight = currHeight)

    override def toString: String = s"Rollback($origHeight->$currHeight, events: ${deferReverted.size})"
  }

  def apply(orig: BlockchainState, event: SubscribeEvent): (BlockchainState, Seq[SubscribeEvent]) = {
    val update = event.getUpdate.update
    val h      = event.getUpdate.height

    val tpe = update match {
      case Update.Append(append) =>
        append.body match {
          case Body.Empty         => "unknown body"
          case _: Body.Block      => "append b"
          case _: Body.MicroBlock => "append mb"
        }
      case _: Update.Rollback => "rollback to"
      case Update.Empty       => "unknown append"
    }
    log.info(s"$orig + $tpe(id=${event.getUpdate.id.toByteStr.take(5)}, h=$h)")

    orig match {
      case orig: Working =>
        update match {
          case _: Update.Append   => (orig.withHeight(Height(h)), Seq(event))
          case _: Update.Rollback => (TransientRollback(orig.height, Height(h), List(event)), Nil)
          case Update.Empty       => (orig.withHeight(Height(h)), Nil)
        }

      case orig: TransientRollback =>
        update match {
          case _: Update.Append =>
            // TODO, microblocks
            if (h >= orig.origHeight) (Working(Height(h)), (event :: orig.deferReverted).reverse)
            else (orig.withAction(event), Nil)

          case _: Update.Rollback => (orig.withAction(event), Nil)
          case Update.Empty       => (orig.withHeight(Height(h)), Nil)
        }
    }
  }
}
