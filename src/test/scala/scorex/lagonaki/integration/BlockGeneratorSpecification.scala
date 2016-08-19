package scorex.lagonaki.integration

import akka.actor.ActorSystem
import akka.testkit._
import org.scalatest.{Matchers, WordSpecLike}
import scorex.consensus.mining.BlockGeneratorController._
import scorex.lagonaki.TestingCommons
import scorex.utils.untilTimeout

import scala.concurrent.duration._

class BlockGeneratorSpecification(_system: ActorSystem)
  extends TestKit(_system)
  with WordSpecLike
  with TestLock
  with ImplicitSender
  with Matchers
  with TestingCommons {

  import TestingCommons._

  def this() = this(ActorSystem("MySpec"))

  val bg = application.blockGenerator

  "BlockGenerator actor" must {

    "generate after downloading state" in {
      bg ! StartGeneration
      //Wait up to 5 seconds to download blockchain and become generating
      untilTimeout(5.seconds) {
        bg ! GetStatus
        expectMsg(Generating.name)
      }
    }

    "StopGeneration command change state to syncing from generating" in {
      bg ! StartGeneration
      untilTimeout(5.seconds) {
        bg ! GetStatus
        expectMsg(Generating.name)
      }
      bg ! StopGeneration
      bg ! GetStatus
      expectMsg(Idle.name)
    }

    "StopGeneration command don't change state from idle" in {
      bg ! StartGeneration
      bg ! StopGeneration
      bg ! GetStatus
      expectMsg(Idle.name)
      bg ! StopGeneration
      bg ! GetStatus
      expectMsg(Idle.name)
    }
  }
}