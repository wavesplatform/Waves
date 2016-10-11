package scorex.waves

import org.scalatest.{BeforeAndAfterAll, Suites}

class WavesSuite extends Suites(
  new DebugAPISpecification,
  new NodeAPISpecification,
  new WavesAPISpecification) with BeforeAndAfterAll {

  import TestingCommons._

  override def beforeAll: Unit ={
    start()
  }

  override def afterAll: Unit ={
    stop()
  }
}
