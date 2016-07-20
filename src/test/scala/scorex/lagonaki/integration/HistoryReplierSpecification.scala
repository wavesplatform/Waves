package scorex.lagonaki.integration

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{Matchers, WordSpecLike}
import scorex.lagonaki.TestingCommons

class HistoryReplierSpecification(_system: ActorSystem)
  extends TestKit(_system)
  with ImplicitSender
  with WordSpecLike
  with Matchers
  with TestingCommons {

  def this() = this(ActorSystem("HistoryReplierSpecification"))

  val probe = new TestProbe(system)

  lazy val application = TestingCommons.application

  //todo: get tests done
  "HistoryReplier actor" must {
    "return block for GetBlock" in {

    }

    "return sigs for GetSignaturesMessage" in {

    }
  }
}
