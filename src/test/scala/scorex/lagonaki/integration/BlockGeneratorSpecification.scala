package scorex.lagonaki.integration

import akka.actor.ActorSystem
import akka.testkit._
import org.scalatest.{Matchers, WordSpecLike}
import scorex.lagonaki.TestingCommons
import scorex.utils.untilTimeout
import scala.concurrent.duration._

//todo: props test: in any state startgenerating->stopgenerating sequence leads to Syncing state
class BlockGeneratorSpecification(_system: ActorSystem)
  extends TestKit(_system)
  with ImplicitSender
  with WordSpecLike
  with Matchers
  with TestingCommons {

  import scorex.network.BlockGenerator._
  import TestingCommons._

  def this() = this(ActorSystem("MySpec"))

  val bg = application.blockGenerator

  "BlockGenerator actor" must {
    "be syncing on start" in {
      bg ! GetStatus
      expectMsg(Syncing.name)
    }

    "generate after downloading state" in {
      bg ! StartGeneration
      //Wait up to 5 seconds to download blockchain and become generating
      untilTimeout(5.seconds) {
        bg ! GetStatus
        expectMsg(Generating.name)
      }
    }
  }
}