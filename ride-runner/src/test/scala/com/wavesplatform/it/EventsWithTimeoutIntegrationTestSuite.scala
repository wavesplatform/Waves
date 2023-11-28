package com.wavesplatform.it

import cats.syntax.option.*
import com.wavesplatform.events.WrappedEvent
import monix.execution.exceptions.UpstreamTimeoutException

import scala.concurrent.duration.DurationInt

class EventsWithTimeoutIntegrationTestSuite extends BaseIntegrationTestSuite {
  "a transaction is received after a timeout if the previous event is" - {
    "a block append" in test(
      events = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(
          mkBlockAppendEvent(
            height = 2,
            forkNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 2L.some))
          )
        ),
        WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
        WrappedEvent.Next(
          mkBlockAppendEvent(
            height = 2,
            forkNumber = 2,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 1L.some))
          )
        ),
        WrappedEvent.Next(mkMicroBlockAppendEvent(2, 1, 2)) // Resolved a synthetic fork
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
        ),
        WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
        WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
        WrappedEvent.Next(
          mkMicroBlockAppendEvent(
            height = 2,
            forkNumber = 2,
            microBlockNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 1L.some))
          )
        )
      ),
      xPlusHeight = 3
    )

    "a rollback" in test(
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
        WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
        WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
        WrappedEvent.Next(
          mkMicroBlockAppendEvent(
            height = 2,
            forkNumber = 2,
            microBlockNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 1L.some))
          )
        ),
        WrappedEvent.Next(mkBlockAppendEvent(3, 2))
      ),
      xPlusHeight = 4
    )
  }

  "a transaction isn't received after a timeout if the previous event is" - {
    "a block append" in test(
      events = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(
          mkBlockAppendEvent(
            height = 2,
            forkNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 1L.some))
          )
        ),
        WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)), // Removes the last block, so we didn't see the data update
        WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
        WrappedEvent.Next(mkMicroBlockAppendEvent(2, 1, 2)) // Resolved a synthetic fork
      ),
      xPlusHeight = 2
    )

    "a micro block" in test(
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
        ),
        WrappedEvent.Next(mkMicroBlockAppendEvent(2, 2, 2)),
        WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)), // Removes the last block, so we didn't see the data update
        WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
        WrappedEvent.Next(mkMicroBlockAppendEvent(2, 1, 2)) // Resolved a synthetic fork
      ),
      xPlusHeight = 2
    )

    "a rollback" - {
      "to a block" in test(
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
          WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
          WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
          WrappedEvent.Next(mkBlockAppendEvent(3, 2)),
          // Because a previous height was 3 and we can't switch to a fork with the less height
          WrappedEvent.Next(mkBlockAppendEvent(4, 2)),
          WrappedEvent.Next(mkMicroBlockAppendEvent(4, 2, 1)) // Resolves a synthetic fork
        ),
        xPlusHeight = 4
      )

      "to a micro block" - {
        "tx in a preserved micro block" in test(
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
            ),
            WrappedEvent.Next(mkMicroBlockAppendEvent(2, 1, 2)),
            WrappedEvent.Next(mkRollbackEvent(2, 1, 1)),
            WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
            WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
            WrappedEvent.Next(mkBlockAppendEvent(3, 2)),
            WrappedEvent.Next(mkMicroBlockAppendEvent(3, 2, 1)) // Resolves a synthetic fork
          ),
          xPlusHeight = 3
        )

        "tx in a removed micro block" in test(
          events = List(
            WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
            WrappedEvent.Next(mkBlockAppendEvent(2, 1)),
            WrappedEvent.Next(mkMicroBlockAppendEvent(2, 1, 1)),
            WrappedEvent.Next(
              mkMicroBlockAppendEvent(
                height = 2,
                forkNumber = 1,
                microBlockNumber = 2,
                dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 1L.some))
              )
            ),
            WrappedEvent.Next(
              mkRollbackEvent(
                height = 2,
                forkNumber = 1,
                microBlockNumber = 1,
                dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", 1L.some, initX.some))
              )
            ),
            WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
            WrappedEvent.Next(mkBlockAppendEvent(2, 2)),
            WrappedEvent.Next(mkBlockAppendEvent(3, 2)),
            WrappedEvent.Next(mkMicroBlockAppendEvent(3, 2, 1)) // Resolves a synthetic fork
          ),
          xPlusHeight = 3
        )
      }
    }
  }

  "a transaction wasn't touched after a timeout if it is in" - {
    "a block" in test(
      events = List(
        WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
        WrappedEvent.Next(
          mkBlockAppendEvent(
            height = 2,
            forkNumber = 1,
            dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 1L.some))
          )
        ),
        WrappedEvent.Next(mkBlockAppendEvent(3, 1)),
        WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)), // Removes the last block, so we didn't see the data update
        WrappedEvent.Next(mkBlockAppendEvent(3, 2))
      ),
      xPlusHeight = 4
    )

    "a rollback" - {
      "to a block" in test(
        events = List(
          WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
          WrappedEvent.Next(
            mkBlockAppendEvent(
              height = 2,
              forkNumber = 1,
              dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 1L.some))
            )
          ),
          WrappedEvent.Next(mkBlockAppendEvent(3, 1)),
          WrappedEvent.Next(mkBlockAppendEvent(4, 1)),
          WrappedEvent.Next(mkRollbackEvent(3, 1)),
          WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
          WrappedEvent.Next(mkBlockAppendEvent(3, 2)),
          WrappedEvent.Next(mkBlockAppendEvent(4, 2))
        ),
        xPlusHeight = 5
      )

      "to a micro block" in test(
        events = List(
          WrappedEvent.Next(mkBlockAppendEvent(1, 1)),
          WrappedEvent.Next(
            mkBlockAppendEvent(
              height = 2,
              forkNumber = 1,
              dataEntryUpdates = List(mkDataEntryUpdate(aliceAddr, "x", initX.some, 1L.some))
            )
          ),
          WrappedEvent.Next(mkBlockAppendEvent(3, 1)),
          WrappedEvent.Next(mkMicroBlockAppendEvent(3, 1, 1)),
          WrappedEvent.Next(mkMicroBlockAppendEvent(3, 1, 2)),
          WrappedEvent.Next(mkRollbackEvent(3, 1, 1)),
          WrappedEvent.Failed(UpstreamTimeoutException(90.seconds)),
          WrappedEvent.Next(mkBlockAppendEvent(3, 2)),
          WrappedEvent.Next(mkBlockAppendEvent(4, 2)),
          // Resolves a fork, we run affected scripts after this
          WrappedEvent.Next(mkMicroBlockAppendEvent(4, 2, 1))
        ),
        xPlusHeight = 5
      )
    }
  }
}
