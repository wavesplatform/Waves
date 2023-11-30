package com.wavesplatform.ride.runner.tests

import com.wavesplatform.ride.runner.RideTestSuite
import com.wavesplatform.ride.runner.RideTestSuite.getTestSuites
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}

import java.io.File
import scala.collection.immutable.SortedSet

class RideTestSuiteTestSuite extends BaseTestSuite with HasTestAccounts {
  private val rootPath = new File("").toPath

  "RideTestSuite" - {
    "getTestSuites" - {
      "one" in {
        val aTest = new File("a.test").toPath
        getTestSuites(List(aTest)) shouldBe RideTestSuite(path = rootPath, testCases = SortedSet(aTest))
      }

      "one nested" in {
        val aTest = new File("foo/bar/a.test").toPath
        getTestSuites(List(aTest)) shouldBe RideTestSuite(
          path = rootPath,
          testSuites = SortedSet(
            RideTestSuite(
              path = new File("foo").toPath,
              testSuites = SortedSet(
                RideTestSuite(
                  path = new File("foo/bar").toPath,
                  testCases = SortedSet(aTest)
                )
              )
            )
          )
        )
      }

      "complex" in {
        val aTest = new File("foo/bar/a.test").toPath
        val bTest = new File("foo/bar/baz/b.test").toPath
        val cTest = new File("bar/c.test").toPath
        val dTest = new File("foo/baz/d.test").toPath

        getTestSuites(List(aTest, bTest, cTest, dTest)) shouldBe RideTestSuite(
          path = rootPath,
          testSuites = SortedSet(
            RideTestSuite(
              path = new File("bar").toPath,
              testCases = SortedSet(cTest)
            ),
            RideTestSuite(
              path = new File("foo").toPath,
              testSuites = SortedSet(
                RideTestSuite(
                  path = new File("foo/bar").toPath,
                  testCases = SortedSet(aTest),
                  testSuites = SortedSet(
                    RideTestSuite(
                      path = new File("foo/bar/baz").toPath,
                      testCases = SortedSet(bTest)
                    )
                  )
                ),
                RideTestSuite(
                  path = new File("foo/baz").toPath,
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
