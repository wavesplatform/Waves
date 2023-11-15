package com.wavesplatform.it

import cats.syntax.option.*
import com.wavesplatform.events.WrappedEvent

class EventsIntegrationTestSuite extends BaseIntegrationTestSuite {
  "a transaction is received in" - {
    "block append" in test(
      events = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(
          mkBlockAppendEvent(
            height = 2,
            forkNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 1L.some))
          )
        )
      ),
      xPlusHeight = 3
    )

    "a micro block append" in test(
      events = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(mkBlockAppendEvent(2, 1)),
        WrappedEvent.Next(
          mkMicroBlockAppendEvent(
            height = 2,
            forkNumber = 1,
            microBlockNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 1L.some))
          )
        )
      ),
      xPlusHeight = 3
    )
  }

  "a rollback happened during" - {
    "a fork" in test(
      events = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(mkBlockAppendEvent(2, 1)),
        WrappedEvent.Next(mkBlockAppendEvent(3, 1)),
        WrappedEvent.Next(
          mkMicroBlockAppendEvent(
            height = 3,
            forkNumber = 1,
            microBlockNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 1L.some))
          )
        ),
        WrappedEvent.Next(
          mkRollbackEvent(
            height = 2,
            forkNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", 1L.some, initX.some))
          )
        ),
        WrappedEvent.Next(mkBlockAppendEvent(3, 2)),
        WrappedEvent.Next(
          mkBlockAppendEvent(
            height = 4,
            forkNumber = 2,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 1L.some))
          )
        ),
        WrappedEvent.Next(mkMicroBlockAppendEvent(4, 2, 1))
      ),
      xPlusHeight = 5
    )

    "a micro fork" in test(
      events = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(mkBlockAppendEvent(2, 1)),
        WrappedEvent.Next(mkMicroBlockAppendEvent(3, 1, 1)),
        WrappedEvent.Next(
          mkMicroBlockAppendEvent(
            height = 3,
            forkNumber = 1,
            microBlockNumber = 2,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 1L.some))
          )
        ),
        WrappedEvent.Next(
          mkRollbackEvent(
            height = 3,
            forkNumber = 1,
            microBlockNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", 1L.some, initX.some))
          )
        ),
        WrappedEvent.Next(mkBlockAppendEvent(4, 1)),
        WrappedEvent.Next(
          mkMicroBlockAppendEvent(
            height = 4,
            forkNumber = 1,
            microBlockNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 1L.some))
          )
        )
      ),
      xPlusHeight = 5
    )
  }
}
