package com.wavesplatform.ride.runner.db

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.state.Height
import org.scalatest.prop.TableDrivenPropertyChecks

class HeightsTestSuite extends BaseTestSuite with TableDrivenPropertyChecks {
  "Heights" - {
    "splitHeightsAt" in forAll(
      Table[Int, Seq[Int], Seq[Int], Seq[Int]](
        ("removeFrom", "heights", "expectedToPreserve", "expectedToRemove"),
        (9, Nil, Nil, Nil),
        (9, Seq(10), Nil, Seq(10)),
        (10, Seq(10), Nil, Seq(10)),
        (11, Seq(10), Seq(10), Nil),
        (11, 12 to 9 by -1, Seq(10, 9), Seq(12, 11))
      )
    ) { (removeFrom, heights, expectedToPreserve, expectedToRemove) =>
      val (toPreserve, toRemove) = Heights.splitHeightsAt(Height(removeFrom), heights.map(Height(_)).toVector)
      withClue("toPreserve")(toPreserve shouldBe expectedToPreserve)
      withClue("toRemove")(toRemove shouldBe expectedToRemove)
    }

    "splitHeightsAtRollback" in {
      forAll(
        Table[Int, Seq[Int], Seq[Int], Seq[Int]](
          ("currentHeight", "heights", "expectedToPreserve", "expectedToRemove"),
          (100, Nil, Nil, Nil),
          (100, 101 to 1 by -1, 101 to 1 by -1, Nil),
          (101, 101 to 1 by -1, 101 to 1 by -1, Nil),
          (102, 101 to 1 by -1, 101 to 2 by -1, Seq(1)),
          (103, 101 to 1 by -1, 101 to 3 by -1, Seq(2, 1)),
          (110, 111 to 1 by -1, 111 to 10 by -1, 9 to 1 by -1)
        )
      ) { (removeFrom, heights, expectedToPreserve, expectedToRemove) =>
        val (toPreserve, toRemove) = Heights.splitHeightsAtRollback(Height(removeFrom), heights.map(Height(_)).toVector)
        withClue("toPreserve")(toPreserve shouldBe expectedToPreserve)
        withClue("toRemove")(toRemove shouldBe expectedToRemove)
      }
    }
  }
}
