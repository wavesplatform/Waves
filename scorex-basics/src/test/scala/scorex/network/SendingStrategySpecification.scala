package scorex.network

import java.net.InetSocketAddress

import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}

import scala.language.{implicitConversions, postfixOps}

class SendingStrategySpecification extends FreeSpec
  with Matchers
  with OneInstancePerTest {

  "SendToChosen should filter (not just return a peer) to avoid non-actual actor usage" in {
    val address = new InetSocketAddress(1111)
    val aPeer = ConnectedPeer(address, null)
    val anActualAlivePeer = ConnectedPeer(address, null)

    val chosen = SendToChosen(aPeer).choose(Seq(anActualAlivePeer)).head

    chosen shouldEqual aPeer
    chosen should be theSameInstanceAs anActualAlivePeer
    chosen should not be theSameInstanceAs(aPeer)
  }

  "BroadcastExceptOf should filter out sender" in {
    val sender = ConnectedPeer(new InetSocketAddress(1111), null)

    val selectedPeers = BroadcastExceptOf(sender).choose(Seq(sender))

    selectedPeers.isEmpty shouldEqual true
  }

  "Broadcast should select all peers except given one" in {
    val sender = ConnectedPeer(new InetSocketAddress(1111), null)
    val peer1 = ConnectedPeer(new InetSocketAddress(2222), null)
    val peer2 = ConnectedPeer(new InetSocketAddress(3333), null)

    val selectedPeers = BroadcastExceptOf(sender).choose(Seq(sender, peer1, peer2))

    selectedPeers.isEmpty shouldEqual false
    selectedPeers.contains(peer1) shouldEqual true
    selectedPeers.contains(peer2) shouldEqual true
    selectedPeers.contains(sender) shouldEqual false
  }


  "Broadcast should work with None passed" in {
    val sender = ConnectedPeer(new InetSocketAddress(1111), null)
    val peer1 = ConnectedPeer(new InetSocketAddress(2222), null)
    val peer2 = ConnectedPeer(new InetSocketAddress(3333), null)

    val selectedPeers = BroadcastExceptOf(null).choose(Seq(sender, peer1, peer2))

    selectedPeers.isEmpty shouldEqual false
    selectedPeers.contains(peer1) shouldEqual true
    selectedPeers.contains(peer2) shouldEqual true
    selectedPeers.contains(sender) shouldEqual true
  }
}
