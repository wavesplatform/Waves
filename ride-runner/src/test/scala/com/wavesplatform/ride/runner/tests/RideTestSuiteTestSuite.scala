package com.wavesplatform.ride.runner.tests

import com.wavesplatform.ride.runner.RideTestSuite
import com.wavesplatform.ride.runner.RideTestSuite.getTestSuites
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}

import java.nio.file.Path
import scala.collection.immutable.SortedSet

class RideTestSuiteTestSuite extends BaseTestSuite with HasTestAccounts {
  private val rootPath = Path.of("")

  "RideTestSuite" - {
    "getTestSuites" - {
      "one" in {
        val aTest = Path.of("a.test")
        getTestSuites(List(aTest)) shouldBe RideTestSuite(path = rootPath, testCases = SortedSet(aTest))
      }

      "one nested" in {
        val aTest = Path.of("foo/bar/a.test")
        getTestSuites(List(aTest)) shouldBe RideTestSuite(
          path = rootPath,
          testSuites = SortedSet(
            RideTestSuite(
              path = Path.of("foo"),
              testSuites = SortedSet(
                RideTestSuite(
                  path = Path.of("foo/bar"),
                  testCases = SortedSet(aTest)
                )
              )
            )
          )
        )
      }

      "complex" in {
        val aTest = Path.of("foo/bar/a.test")
        val bTest = Path.of("foo/bar/baz/b.test")
        val cTest = Path.of("bar/c.test")
        val dTest = Path.of("foo/baz/d.test")

        getTestSuites(List(aTest, bTest, cTest, dTest)) shouldBe RideTestSuite(
          path = rootPath,
          testSuites = SortedSet(
            RideTestSuite(
              path = Path.of("bar"),
              testCases = SortedSet(cTest)
            ),
            RideTestSuite(
              path = Path.of("foo"),
              testSuites = SortedSet(
                RideTestSuite(
                  path = Path.of("foo/bar"),
                  testCases = SortedSet(aTest),
                  testSuites = SortedSet(
                    RideTestSuite(
                      path = Path.of("foo/bar/baz"),
                      testCases = SortedSet(bTest)
                    )
                  )
                ),
                RideTestSuite(
                  path = Path.of("foo/baz"),
                  testCases = SortedSet(dTest)
                )
              )
            )
          )
        )
      }
    }
  }
}
