package com.wavesplatform.blockchain

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated
import com.wavesplatform.state.Height

class BlockchainStateTestSuite extends BaseTestSuite {
  "BlockchainState.apply" - {
    "append" - {
      "block" in {
        val event = mkBlockAppendEvent(11)
        val (updated, events) = BlockchainState(BlockchainState.Working(Height(10)), event)

        updated shouldBe a[BlockchainState.Working]
        events shouldBe List(event)
      }

      "micro block" in {
        val event = mkMicroBlockAppendEvent(11)
        val (updated, events) = BlockchainState(BlockchainState.Working(Height(10)), event)

        updated shouldBe a[BlockchainState.Working]
        events shouldBe List(event)
      }
    }

    "rollback" - {
      "doesn't resolve after reaching an origin fork height + 1" in {
        val origEvents = List(mkBlockAppendEvent(10), mkRollbackEvent(9))
        val orig       = BlockchainState.RollingBack(Height(10), Height(11), 0, origEvents)

        val event        = mkBlockAppendEvent(11)
        val (updated, _) = BlockchainState(orig, event)

        updated shouldBe a[BlockchainState.RollingBack]
      }

      "resolve after" - {
        "reaching an origin fork height + 2" in {
          val origEvents = List(mkBlockAppendEvent(11), mkBlockAppendEvent(10), mkRollbackEvent(9))
          val orig       = BlockchainState.RollingBack(Height(10), Height(11), 0, origEvents)

          val event             = mkBlockAppendEvent(12)
          val (updated, events) = BlockchainState(orig, event)

          updated shouldBe a[BlockchainState.Working]
          events shouldBe (event :: origEvents).reverse
        }

        "reaching an origin fork height + 1 and getting a micro block" in {
          val origEvents = List(mkBlockAppendEvent(11), mkBlockAppendEvent(10), mkRollbackEvent(9))
          val orig       = BlockchainState.RollingBack(Height(10), Height(11), 0, origEvents)

          val event             = mkMicroBlockAppendEvent(11)
          val (updated, events) = BlockchainState(orig, event)

          updated shouldBe a[BlockchainState.Working]
          events shouldBe (event :: origEvents).reverse
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
