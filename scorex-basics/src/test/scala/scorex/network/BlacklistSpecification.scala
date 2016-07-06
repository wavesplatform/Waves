package scorex.network

import java.net.{InetAddress, InetSocketAddress}

import org.scalatest.{FeatureSpec, GivenWhenThen}
import play.api.libs.json.{JsObject, Json}
import scorex.network.peer.{PeerDatabaseImpl, PeerInfo}
import scorex.settings.Settings

class BlacklistSpecification extends FeatureSpec with GivenWhenThen {

  object TestSettings extends Settings {
    override lazy val settingsJSON: JsObject = Json.obj()
    override val filename: String = ""
    override lazy val blacklistResidenceTimeMilliseconds = 1000L
  }

  info("As a Peer")
  info("I want to blacklist other peers for certain time")
  info("So I can give them another chance after")

  feature("Blacklist") {
    scenario("Peer blacklist another peer") {

      Given("Peer database is empty")
      val peerDatabase = new PeerDatabaseImpl(TestSettings, None)
      assert(peerDatabase.knownPeers(false).isEmpty)
      assert(peerDatabase.blacklistedPeers().isEmpty)

      When("Peer adds another peer to whitelist")
      val anotherPeer = new PeerInfo(System.currentTimeMillis)
      val port: Int = 1234
      val address = new InetSocketAddress(InetAddress.getByName("localhost"), port)
      peerDatabase.addOrUpdateKnownPeer(address, anotherPeer)
      assert(peerDatabase.knownPeers(false).contains(address))
      assert(!peerDatabase.blacklistedPeers().contains(address.getHostName))

      And("Peer blacklists another peer")
      peerDatabase.blacklistPeer(address)
      assert(peerDatabase.isBlacklisted(address))
      assert(peerDatabase.blacklistedPeers().contains(address.getHostName))
      assert(!peerDatabase.knownPeers(false).contains(address))

      And("Peer waits for some time")
      Thread.sleep(TestSettings.blacklistResidenceTimeMilliseconds)

      Then("Another peer disappear from blacklist")
      assert(!peerDatabase.isBlacklisted(address))

      And("Another peer still not in whitelist")
      assert(!peerDatabase.knownPeers(false).contains(address))
    }
  }
}
