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
      assert(peerDatabase.blacklisted.isEmpty)

      When("Peer adds another peer to knownPeers")
      val anotherPeer = new PeerInfo(System.currentTimeMillis)
      val port: Int = 1234
      val address = new InetSocketAddress(InetAddress.getByName("localhost"), port)
      peerDatabase.addOrUpdateKnownPeer(address, anotherPeer)
      assert(peerDatabase.knownPeers(false).contains(address))
      assert(!peerDatabase.blacklisted.contains(address))

      And("Peer blacklists another peer")
      peerDatabase.blacklist(address)
      assert(peerDatabase.isBlacklisted(address))
      assert(peerDatabase.blacklisted.contains(address))
      assert(!peerDatabase.knownPeers(false).contains(address))

      And("Peer waits for some time")
      Thread.sleep(TestSettings.blacklistResidenceTimeMilliseconds)

      Then("Another peer disappear from blacklist")
      assert(!peerDatabase.isBlacklisted(address))

      And("Another peer became known")
      assert(peerDatabase.knownPeers(false).contains(address))
    }
  }
}
