package scorex.network.peer

import java.net.InetSocketAddress

import org.scalatest.{FlatSpec, Matchers}

class PeerRegisterSpecification extends FlatSpec with Matchers {
  val address1 = new InetSocketAddress("1.1.1.1", 1)
  val address2 = new InetSocketAddress("2.2.2.2", 2)
  val address3 = new InetSocketAddress("3.3.3.3", 3)

  "PeerRegister" should "store information about peer during the process of establishing connection" in {
    val register = new PeerRegister()

    register.initiateOutboundConnection(address1)
    register.isRegistered(address1) should be(true)
    register.isRegistered(address2) should be(false)
  }

  it should "store addresses of peers in the middle connecting" in {
    val register = new PeerRegister()

    register.initiateOutboundConnection(address1)
    register.initiateOutboundConnection(address2)
    register.isRegistered(address1) should be(true)
    register.isRegistered(address2) should be(true)
    register.isRegistered(address3) should be(false)
  }

  it should "correctly reply with presence of peers in the middle of connection establishing process" in {
    val register = new PeerRegister()
    register.initiateOutboundConnection(address1)
  }
}
