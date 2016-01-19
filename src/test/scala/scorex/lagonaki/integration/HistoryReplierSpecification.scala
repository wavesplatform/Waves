package scorex.lagonaki.integration

import akka.actor.{Props, ActorSystem}
import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import org.scalatest.{Matchers, WordSpecLike}
import scorex.lagonaki.TestingCommons
import scorex.network.HistorySynchronizer

class HistoryReplierSpecification(_system: ActorSystem)
  extends TestKit(_system)
  with ImplicitSender
  with WordSpecLike
  with Matchers
  with TestingCommons {

  def this() = this(ActorSystem("HistoryReplierSpecification"))

  val probe = new TestProbe(system)

  val hs = system.actorOf(Props(classOf[HistorySynchronizer], application))

  lazy val application = TestingCommons.application

  //todo: get tests done
  "HistoryReplier actor" must {
    "return block for GetBlock" in {

    }

    "return sigs for GetSignaturesMessage" in {

    }
  }
}
