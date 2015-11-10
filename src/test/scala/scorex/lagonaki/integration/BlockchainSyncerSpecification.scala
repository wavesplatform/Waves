package scorex.lagonaki.integration

import akka.actor.ActorSystem
import akka.testkit._
import org.scalatest.{Matchers, WordSpecLike}
import scorex.lagonaki.network.BlockchainSyncer.{Generating, GetStatus, Offline}
import scorex.lagonaki.server.LagonakiApplication
import scorex.utils.untilTimeout
import scala.concurrent.duration._

class BlockchainSyncerSpecification(_system: ActorSystem)
  extends TestKit(_system)
  with ImplicitSender
  with WordSpecLike
  with Matchers {

  def this() = this(ActorSystem("MySpec"))

  val application = new LagonakiApplication("settings-test.json")
  val bcs = application.blockchainSyncer

  "BlockchainSyncer actor" must {
    "be offline on load" in {
      bcs ! GetStatus
      expectMsg(Offline.name)
    }
    "generate after downloading state" in {
      bcs ! Unit
      //Wait up to 5 seconds to download blockchain and become generating
      untilTimeout(5.seconds) {
        bcs ! GetStatus
        expectMsg(Generating.name)
      }
    }
  }
}