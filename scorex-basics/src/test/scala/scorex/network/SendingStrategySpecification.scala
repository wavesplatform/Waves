package scorex.network

import java.net.InetSocketAddress

import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}

import scala.language.{implicitConversions, postfixOps}

class SendingStrategySpecification extends FreeSpec
  with Matchers
  with OneInstancePerTest {

  "SendToChosen should filter, not just return a peer to avoid non-actual actor usage" in {
    val address = new InetSocketAddress(1111)
    val aPeer = ConnectedPeer(address, null)
    val anActualAlivePeer = ConnectedPeer(address, null)

    val chosen = SendToChosen(aPeer).choose(Seq(anActualAlivePeer)).head

    chosen shouldEqual aPeer
    chosen should be theSameInstanceAs anActualAlivePeer
    chosen should not be theSameInstanceAs (aPeer)
  }
}
