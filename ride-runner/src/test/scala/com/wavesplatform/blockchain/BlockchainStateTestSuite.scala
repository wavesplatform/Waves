package com.wavesplatform.blockchain

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.ride.runner.BlockchainState
import com.wavesplatform.state.Height

class BlockchainStateTestSuite extends BaseTestSuite {
  "BlockchainState.apply" - {
    "Starting" - {
      "append" - {
        "reaching the blockchain height - become Working, process the event and run scripts" in {
          val event     = mkBlockAppendEvent(10)
          val processor = new TestProcessor

          val updatedState = BlockchainState(processor, BlockchainState.Starting(Height(9), Height(10)), event)
          updatedState shouldBe a[BlockchainState.Working]
          processor.actions shouldBe Vector(RemoveFrom(10), Process(event))
        }

        "not reaching the blockchain height - still Starting" in {
          val event        = mkBlockAppendEvent(9)
          val updatedState = BlockchainState(new TestProcessor, BlockchainState.Starting(Height(9), Height(10)), event)
          updatedState shouldBe a[BlockchainState.Starting]
        }

        "not found a block - remove all data on higher heights and process the event" in {
          val event     = mkBlockAppendEvent(9)
          val processor = new TestProcessor

          val _ = BlockchainState(processor, BlockchainState.Starting(Height(8), Height(10)), event)
          processor.actions shouldBe Vector(RemoveFrom(9), Process(event))
        }

        "found the same block - ignore" in {
          val event = mkBlockAppendEvent(9)
          val processor = new TestProcessor {
            override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] = Some(true)
          }

          val _ = BlockchainState(processor, BlockchainState.Starting(Height(8), Height(10)), event)
          processor.actions shouldBe Vector.empty
        }

        "found a difference - remove old fork data and process the event" in {
          val event = mkBlockAppendEvent(9)
          val processor = new TestProcessor {
            override def hasLocalBlockAt(height: Height, id: ByteStr): Option[Boolean] = Some(false)
          }

          val _ = BlockchainState(processor, BlockchainState.Starting(Height(8), Height(10)), event)
          processor.actions shouldBe Vector(
            RemoveFrom(event),
            Process(event)
          )
        }
      }

      "rollback - process the event" in {
        val event     = mkRollbackEvent(1)
        val processor = new TestProcessor

        val updatedState = BlockchainState(processor, BlockchainState.Starting(Height(9), Height(10)), event)
        updatedState shouldBe a[BlockchainState.Starting]
        processor.actions shouldBe Vector(Process(event))
      }
    }

    "Working" - {
      "append" - {
        "block - process the event and run scripts" in {
          val event     = mkBlockAppendEvent(11)
          val processor = new TestProcessor

          val updatedState = BlockchainState(processor, BlockchainState.Working(Height(10)), event)
          updatedState shouldBe a[BlockchainState.Working]
          processor.actions shouldBe Vector(Process(event), RunAffectedScripts)
        }

        "micro block - process the event and run scripts" in {
          val event     = mkMicroBlockAppendEvent(10)
          val processor = new TestProcessor

          val updatedState = BlockchainState(processor, BlockchainState.Working(Height(10)), event)
          updatedState shouldBe a[BlockchainState.Working]
          processor.actions shouldBe Vector(Process(event), RunAffectedScripts)
        }
      }

      "rollback - become ResolvingFork, process the event" in {
        val event     = mkRollbackEvent(1)
        val processor = new TestProcessor

        val updatedState = BlockchainState(processor, BlockchainState.Working(Height(10)), event)
        updatedState shouldBe a[BlockchainState.ResolvingFork]
        processor.actions shouldBe Vector(Process(event))
      }
    }

    "ResolvingFork" - {
      "rollback - remove old fork data and process the event" in {
        val event     = mkRollbackEvent(1)
        val processor = new TestProcessor

        val updatedState = BlockchainState(processor, BlockchainState.ResolvingFork(Height(10), 0, Height(10)), event)
        updatedState shouldBe a[BlockchainState.ResolvingFork]
        processor.actions shouldBe Vector(Process(event))
      }

      "fork resolution" - {
        "doesn't resolve after reaching an origin fork height + 1" in {
          val event     = mkBlockAppendEvent(11)
          val processor = new TestProcessor

          val updatedState = BlockchainState(
            processor,
            BlockchainState.ResolvingFork.from(rollbackHeight = Height(10), origForkHeight = Height(10)),
            event
          )

          isA[BlockchainState.ResolvingFork](updatedState).isRollbackResolved shouldBe false
          processor.actions shouldBe Vector(Process(event))
        }

        "resolve after and run scripts" - {
          "reaching an origin fork height + 2" in {
            val event     = mkBlockAppendEvent(12)
            val processor = new TestProcessor

            val updatedState = BlockchainState(processor, BlockchainState.ResolvingFork(Height(11), 0, Height(10)), event)
            updatedState shouldBe a[BlockchainState.Working]
            processor.actions shouldBe Vector(Process(event), RunAffectedScripts)
          }

          "reaching an origin fork height + 1 and getting a micro block" in {
            val event     = mkMicroBlockAppendEvent(11)
            val processor = new TestProcessor

            val updatedState = BlockchainState(processor, BlockchainState.ResolvingFork(Height(11), 0, Height(10)), event)
            updatedState shouldBe a[BlockchainState.Working]
            processor.actions shouldBe Vector(Process(event), RunAffectedScripts)
          }
        }
      }
    }
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
