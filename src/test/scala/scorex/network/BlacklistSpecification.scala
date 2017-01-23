package scorex.network

import java.net.{InetAddress, InetSocketAddress}

import org.scalatest.{FeatureSpec, GivenWhenThen}
import play.api.libs.json.{JsObject, Json}
import scorex.network.peer.PeerDatabaseImpl
import scorex.settings.Settings

class BlacklistSpecification extends FeatureSpec with GivenWhenThen {

  object TestSettings extends Settings {
    override lazy val settingsJSON: JsObject = Json.obj()
    override lazy val blacklistResidenceTimeMilliseconds = 1000L
  }

  info("As a Peer")
  info("I want to blacklist other peers for certain time")
  info("So I can give them another chance after")

  feature("Blacklist") {
    scenario("Peer blacklist another peer") {

      Given("Peer database is empty")
      val peerDatabase = new PeerDatabaseImpl(TestSettings, None)
      def isBlacklisted(address: InetSocketAddress) = peerDatabase.getBlacklist.contains(address.getHostName)

      assert(peerDatabase.getKnownPeers.isEmpty)
      assert(peerDatabase.getBlacklist.isEmpty)

      When("Peer adds another peer to knownPeers")
      val address = new InetSocketAddress(InetAddress.getByName("localhost"), 1234)
      peerDatabase.addPeer(address, Some(0), None)
      assert(peerDatabase.getKnownPeers.contains(address))
      assert(!isBlacklisted(address))

      And("Peer blacklists another peer")
      peerDatabase.blacklistHost(address.getHostName)
      assert(isBlacklisted(address))
      assert(!peerDatabase.getKnownPeers.contains(address))

      And("Peer waits for some time")
      Thread.sleep(TestSettings.blacklistResidenceTimeMilliseconds)

      Then("Another peer disappear from blacklist")
      assert(!isBlacklisted(address))

      And("Another peer became known")
      assert(peerDatabase.getKnownPeers.contains(address))
    }
  }
}
