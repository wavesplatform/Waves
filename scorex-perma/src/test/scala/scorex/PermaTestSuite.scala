package scorex

import org.scalatest.Suites

class PermaTestSuite extends Suites (
  new props.AuthDataStorageSpecification
)
