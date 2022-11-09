package com.wavesplatform.blockchain

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

  case class RollingBack(origHeight: Height, currHeight: Height, microBlockNumber: Int, deferReverted: List[SubscribeEvent]) extends BlockchainState {
    def withAction(action: SubscribeEvent): RollingBack =
      copy(
        deferReverted = action :: deferReverted,
        currHeight = Height(action.getUpdate.height),
        microBlockNumber = action.getUpdate.update match {
          case Update.Empty       => microBlockNumber
          case _: Update.Rollback => 0
          case Update.Append(append) =>
            append.body match {
              case Body.Empty         => microBlockNumber
              case _: Body.Block      => 0
              case _: Body.MicroBlock => microBlockNumber + 1
            }
        }
      )

    def withHeight(currHeight: Height): RollingBack = copy(currHeight = currHeight, microBlockNumber = 0)

    def isRollbackResolved: Boolean = {
      val resolveHeight = origHeight + 1
      // Rollback is resolved, when the new fork height > an old fork height. But we will wait a micro block or a next block.
      currHeight > resolveHeight ||
      currHeight == resolveHeight && microBlockNumber >= 1
    }

    override def toString: String = s"Rollback($origHeight->$currHeight, events: ${deferReverted.size})"
  }

  object RollingBack {
    def from(origHeight: Height, event: SubscribeEvent): RollingBack =
      new RollingBack(
        origHeight = origHeight,
        currHeight = Height(event.getUpdate.height),
        microBlockNumber = 0,
        deferReverted = List(event)
      )
  }

  def apply(orig: BlockchainState, event: SubscribeEvent): (BlockchainState, Seq[SubscribeEvent]) = {
    val update = event.getUpdate.update
    val h      = Height(event.getUpdate.height)

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
          case _: Update.Append   => (orig.withHeight(h), List(event))
          case _: Update.Rollback => (RollingBack.from(orig.height, event), Nil)
          case Update.Empty       => (orig.withHeight(h), Nil)
        }

      case orig: RollingBack =>
        update match {
          case _: Update.Append =>
            val updated = orig.withAction(event)
            if (updated.isRollbackResolved) (Working(updated.currHeight), updated.deferReverted.reverse)
            else (updated, Nil)

          case _: Update.Rollback => (orig.withAction(event), Nil)
          case Update.Empty       => (orig.withHeight(h), Nil)
        }
    }
  }
}
