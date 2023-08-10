package com.wavesplatform.ride.runner

import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.mutable

case class RideTestSuite(path: Path, testCases: SortedSet[Path] = SortedSet.empty, testSuites: SortedSet[RideTestSuite] = SortedSet.empty) {
  def withTestSuite(testSuite: RideTestSuite): RideTestSuite = copy(testSuites = testSuites + testSuite)
  def withTestCase(testCase: Path): RideTestSuite            = copy(testCases = testCases + testCase)
}

object RideTestSuite {
  implicit val rideTestSuiteOrdering: Ordering[RideTestSuite] = Ordering.by[RideTestSuite, Path](_.path)

  def getTestSuites(testCases: List[Path]): RideTestSuite = {
    val testSuites = mutable.Map.empty[Path, RideTestSuite]

    val rootPath = Path.of("") // Dummy path if test cases just file names
    testSuites.put(rootPath, RideTestSuite(path = rootPath))

    // Adding test cases
    testCases.foreach { testCase =>
      val parents = allParents(testCase)
      parents.filterNot(testSuites.contains).foreach { newParent =>
        testSuites.put(newParent, RideTestSuite(path = newParent))
      }

      val directParent = parents.headOption.getOrElse(rootPath)
      testSuites.updateWith(directParent)(_.map(_.withTestCase(testCase)))
    }

    // Folding test suites
    testSuites.keys.toVector
      .sortBy(_.getNameCount)(Ordering.ordered[Int].reverse) // Deepest in the front
      .foreach { testSuitePath =>
        if (testSuitePath != rootPath) {
          val testSuite       = testSuites(testSuitePath)
          val testSuiteParent = Option(testSuite.path.getParent).getOrElse(rootPath)
          testSuites.updateWith(testSuiteParent)(_.map(_.withTestSuite(testSuite)))
        }
      }

    testSuites(rootPath)
  }

  @tailrec def minimize(currRoot: RideTestSuite): RideTestSuite =
    if (currRoot.testCases.isEmpty && currRoot.testSuites.size == 1) minimize(currRoot.testSuites.head)
    else currRoot

  def allParents(path: Path): Vector[Path] = {
    @tailrec def loop(currParent: Path, parents: Vector[Path]): Vector[Path] =
      if (currParent == null) parents
      else loop(currParent.getParent, parents :+ currParent)

    loop(path.getParent, Vector.empty)
  }
}
