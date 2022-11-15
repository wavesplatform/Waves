package com.wavesplatform.blockchain

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.blockchain.BlockchainStateTestSuite.EmptyProcessor
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.state.Height
import BlockchainStateTestSuite.*

class BlockchainStateTestSuite extends BaseTestSuite {
  "BlockchainState.apply" - {
    "Starting" - {
      "append" - {
        "reaching the blockchain height - become Working, process the event and run all scripts" in {
          val event = mkBlockAppendEvent(10)
          val processor = new EmptyProcessor

          val updatedState = BlockchainState(processor, BlockchainState.Starting(Height(10)), event)
          updatedState shouldBe a[BlockchainState.Working]
          processor.actions shouldBe Vector(
            Process(event),
            RunScripts(forceAll = false)
          )
        }

        "not reaching the blockchain height - still Starting" in {
          val event = mkBlockAppendEvent(9)
          val updatedState = BlockchainState(new EmptyProcessor, BlockchainState.Starting(Height(10)), event)
          updatedState shouldBe a[BlockchainState.Starting]
        }

        "not found a block - only process the event" in {
          val event = mkBlockAppendEvent(9)
          val processor = new EmptyProcessor

          val _ = BlockchainState(processor, BlockchainState.Starting(Height(10)), event)
          processor.actions shouldBe Vector(Process(event))
        }

        "found the same block - only process the event" in {
          val event = mkBlockAppendEvent(9)
          val processor = new EmptyProcessor {
            override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] = Some(true)
          }

          val _ = BlockchainState(processor, BlockchainState.Starting(Height(10)), event)
          processor.actions shouldBe Vector(Process(event))
        }

        "found a difference - remove old fork data and process the event" in {
          val event = mkBlockAppendEvent(9)
          val processor = new EmptyProcessor {
            override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] = Some(false)
          }

          val _ = BlockchainState(processor, BlockchainState.Starting(Height(10)), event)
          processor.actions shouldBe Vector(
            RemoveFrom(event),
            Process(event)
          )
        }
      }

      "rollback - throwing an exception" in {
        val event = mkRollbackEvent(1)
        assertThrows[IllegalStateException] {
          BlockchainState(new EmptyProcessor, BlockchainState.Starting(Height(10)), event)
        }
      }
    }

    "Working" - {}
    "ResolvingFork" - {}

    //    "Append" - {
    //      "block" in {
    //        val event = mkBlockAppendEvent(11)
    //        val (updated, events) = BlockchainState(BlockchainState.Working(Height(10)), event)
    //
    //        updated shouldBe a[BlockchainState.Working]
    //        events shouldBe List(event)
    //      }
    //
    //      "micro block" in {
    //        val event = mkMicroBlockAppendEvent(11)
    //        val (updated, events) = BlockchainState(BlockchainState.Working(Height(10)), event)
    //
    //        updated shouldBe a[BlockchainState.Working]
    //        events shouldBe List(event)
    //      }
    //    }
    //
    //    "Rollback" - {
    //      "doesn't resolve after reaching an origin fork height + 1" in {
    //        val origEvents = List(mkBlockAppendEvent(10), mkRollbackEvent(9))
    //        val orig       = BlockchainState.RollingBack(Height(10), Height(11), 0, origEvents)
    //
    //        val event        = mkBlockAppendEvent(11)
    //        val (updated, _) = BlockchainState(orig, event)
    //
    //        updated shouldBe a[BlockchainState.ResolvingFork]
    //      }
    //
    //      "resolve after" - {
    //        "reaching an origin fork height + 2" in {
    //          val origEvents = List(mkBlockAppendEvent(11), mkBlockAppendEvent(10), mkRollbackEvent(9))
    //          val orig       = BlockchainState.RollingBack(Height(10), Height(11), 0, origEvents)
    //
    //          val event             = mkBlockAppendEvent(12)
    //          val (updated, events) = BlockchainState(orig, event)
    //
    //          updated shouldBe a[BlockchainState.Working]
    //          events shouldBe (event :: origEvents).reverse
    //        }
    //
    //        "reaching an origin fork height + 1 and getting a micro block" in {
    //          val origEvents = List(mkBlockAppendEvent(11), mkBlockAppendEvent(10), mkRollbackEvent(9))
    //          val orig       = BlockchainState.RollingBack(Height(10), Height(11), 0, origEvents)
    //
    //          val event             = mkMicroBlockAppendEvent(11)
    //          val (updated, events) = BlockchainState(orig, event)
    //
    //          updated shouldBe a[BlockchainState.Working]
    //          events shouldBe (event :: origEvents).reverse
    //        }
    //      }
    //    }
  }

  private def mkRollbackEvent(height: Int): SubscribeEvent = SubscribeEvent().withUpdate(
    BlockchainUpdated()
      .withHeight(height)
      .withUpdate(
        BlockchainUpdated.Update.Rollback(
          BlockchainUpdated.Rollback()
        )
      )
  )

  private def mkMicroBlockAppendEvent(height: Int): SubscribeEvent = SubscribeEvent().withUpdate(
    BlockchainUpdated()
      .withHeight(height)
      .withUpdate(
        BlockchainUpdated.Update.Append(
          BlockchainUpdated
            .Append()
            .withMicroBlock(
              BlockchainUpdated.Append.MicroBlockAppend()
            )
        )
      )
  )

  private def mkBlockAppendEvent(height: Int): SubscribeEvent = SubscribeEvent().withUpdate(
    BlockchainUpdated()
      .withHeight(height)
      .withUpdate(
        BlockchainUpdated.Update.Append(
          BlockchainUpdated
            .Append()
            .withBlock(
              BlockchainUpdated.Append.BlockAppend()
            )
        )
      )
  )
}

object BlockchainStateTestSuite {
  private class EmptyProcessor extends Processor {
    override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] = {
      // events = events.appended(HasLocalBlockAt(height, id))
      None
    }

    override def removeFrom(height: Height): Unit = {
      actions = actions.appended(RemoveFrom(height))
    }

    override def process(event: BlockchainUpdated): Unit = {
      actions = actions.appended(Process(event))
    }

    override def runScripts(forceAll: Boolean): Unit = {
      actions = actions.appended(RunScripts(forceAll))
    }

    var actions: Vector[ProcessorAction] = Vector.empty
  }

  private sealed trait ProcessorAction extends Product with Serializable

  private case class RemoveFrom(height: Int) extends ProcessorAction

  private object RemoveFrom {
    def apply(event: SubscribeEvent): RemoveFrom = RemoveFrom(event.getUpdate.height)
  }

  private case class Process(updated: BlockchainUpdated) extends ProcessorAction

  private object Process {
    def apply(event: SubscribeEvent): Process = Process(event.getUpdate)
  }

  private case class RunScripts(forceAll: Boolean) extends ProcessorAction
}
