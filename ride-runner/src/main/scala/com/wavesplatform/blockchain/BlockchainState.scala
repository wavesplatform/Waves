package com.wavesplatform.blockchain

import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging

// TODO doesn't relate to blockchain itself, move to the business domain
sealed trait BlockchainState extends Product with Serializable

object BlockchainState extends ScorexLogging {
  case class Starting(blockchainHeight: Height, foundDifference: Boolean = false) extends BlockchainState {
    def withFoundDifference: Starting = copy(foundDifference = true)
  }

  case class Working(height: Height) extends BlockchainState {
    def withHeight(height: Height): Working = copy(height = height)

    override def toString: String = s"Working($height)"
  }

  case class RollingBack(origHeight: Height, currHeight: Height, microBlockNumber: Int) extends BlockchainState {
    def withAction(action: SubscribeEvent): RollingBack =
      copy(
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

    override def toString: String = s"Rollback($origHeight->$currHeight, mbn: $microBlockNumber)"
  }

  object RollingBack {
    def from(origHeight: Height, event: SubscribeEvent): RollingBack =
      new RollingBack(
        origHeight = origHeight,
        currHeight = Height(event.getUpdate.height),
        microBlockNumber = 0
      )
  }

  def apply(processor: Processor, orig: BlockchainState, event: SubscribeEvent): BlockchainState = {
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
    val currBlockId = event.getUpdate.id.toByteStr
    log.info(s"$orig + $tpe(id=${currBlockId.take(5)}, h=$h)")

    orig match {
      case orig: Starting =>
        val comparedBlocks =
          if (processor.hasLocalBlockAt(h, currBlockId) || orig.foundDifference) orig
          else {
            processor.removeFrom(h)
            orig.withFoundDifference
          }

        processor.process(h, event.getUpdate)
        if (h >= comparedBlocks.blockchainHeight) {
          log.debug(s"[$h] Reached the current height, run all scripts")
          processor.runScripts()
          Working(h)
        } else comparedBlocks

      case orig: Working =>
        update match {
          case _: Update.Append =>
            processor.process(h, event.getUpdate)
            processor.runScripts()
            orig.withHeight(h)

          case _: Update.Rollback =>
            processor.removeFrom(Height(h + 1))
            processor.process(h, event.getUpdate)
            RollingBack.from(orig.height, event)

          case Update.Empty => orig.withHeight(h)
        }

      case orig: RollingBack =>
        update match {
          case _: Update.Append =>
            processor.process(h, event.getUpdate)
            val updated = orig.withAction(event)
            if (updated.isRollbackResolved) {
              processor.runScripts()
              Working(updated.currHeight)
            } else updated

          case _: Update.Rollback =>
            processor.removeFrom(Height(h + 1))
            processor.process(h, event.getUpdate)
            orig.withAction(event)

          case Update.Empty => orig.withHeight(h)
        }
    }
  }
}
