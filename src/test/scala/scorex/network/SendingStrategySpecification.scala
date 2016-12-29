package scorex.network

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}

import scala.language.{implicitConversions, postfixOps}

class SendingStrategySpecification extends FreeSpec
  with Matchers
  with MockFactory
  with OneInstancePerTest {

  "SendToChosen" in {
    val nonce: Long = 333

    val aPeer = stub[ConnectedPeer]
    (aPeer.nonce _).when().returns(nonce)

    val chosen = SendToChosen(aPeer).choose(Seq((nonce, ()))).head

    chosen shouldBe (nonce, ())
  }

  "BroadcastExceptOf" - {
    def connectedPeer(nonce: Long): ConnectedPeer = {
      val peer = stub[ConnectedPeer]
      (peer.nonce _).when().returns(nonce)
      peer
    }

    def peers(connectPeers: ConnectedPeer*): Seq[(Long, Unit)] = connectPeers.map(peer => (peer.nonce, ()))
    def toNonces(values: Seq[(Long, _)]): Seq[Long] = values.map(_._1)

    val sender = connectedPeer(1111)
    val peer1 = connectedPeer(2222)
    val peer2 = connectedPeer(3333)

    "should filter out sender" in {
      val selectedPeers = BroadcastExceptOf(sender).choose(peers(sender))

      selectedPeers.isEmpty shouldEqual true
    }

    "should select all peers except given one" in {
      val selected = toNonces(BroadcastExceptOf(sender).choose(peers(sender, peer1, peer2)))

      selected.isEmpty shouldEqual false
      selected.contains(peer1.nonce) shouldEqual true
      selected.contains(peer2.nonce) shouldEqual true
      selected.contains(sender.nonce) shouldEqual false
    }


    "should work with None passed" in {
      val selected = toNonces(BroadcastExceptOf(null).choose(peers(sender, peer1, peer2)))

      selected.isEmpty shouldEqual false
      selected.contains(peer1.nonce) shouldEqual true
      selected.contains(peer2.nonce) shouldEqual true
      selected.contains(sender.nonce) shouldEqual true
    }
  }
}
