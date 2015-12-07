package scorex.lagonaki.integration

import akka.actor.ActorSystem
import akka.testkit._
import org.scalatest.{Matchers, WordSpecLike}
import scorex.lagonaki.server.LagonakiApplication
import scorex.TimeUtils.untilTimeout
import scala.concurrent.duration._

//todo: props test: in any state startgenerating->stopgenerating sequence leads to Syncing state
class BlockGeneratorSpecification(_system: ActorSystem)
  extends TestKit(_system)
  with ImplicitSender
  with WordSpecLike
  with Matchers {

  import scorex.network.BlockGenerator._

  def this() = this(ActorSystem("MySpec"))

  val application = new LagonakiApplication("settings-test.json")
  val bcs = application.blockGenerator

  "BlockGenerator actor" must {
    "be syncing on start" in {
      bcs ! GetStatus
      expectMsg(Syncing.name)
    }

    "generate after downloading state" in {
      bcs ! StartGeneration
      //Wait up to 5 seconds to download blockchain and become generating
      untilTimeout(5.seconds) {
        bcs ! GetStatus
        expectMsg(Generating.name)
      }
    }
  }
}