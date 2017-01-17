package scorex.waves

import com.wavesplatform.matcher.integration.MatcherAPISpecification
import com.wavesplatform.matcher.market.OrderBookActorSpecification
import com.wavesplatform.matcher.model.EventJsonSpecification
import org.scalatest.{BeforeAndAfterAll, Suites}

class WavesSuite extends Suites(
  new DebugAPISpecification,
  new NodeAPISpecification,
  new WavesAPISpecification,
  new MatcherAPISpecification
) with BeforeAndAfterAll {

  import TestingCommons._

  override def beforeAll: Unit ={
    start()
  }

  override def afterAll: Unit ={
    stop()
  }
}
