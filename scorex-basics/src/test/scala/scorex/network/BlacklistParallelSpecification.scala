package scorex.network

import java.net.{InetAddress, InetSocketAddress}

import org.scalatest.{FeatureSpec, GivenWhenThen, ParallelTestExecution}
import play.api.libs.json.{JsObject, Json}
import scorex.network.peer.{PeerDatabaseImpl, PeerInfo}
import scorex.settings.Settings

class BlacklistParallelSpecification extends FeatureSpec with GivenWhenThen with ParallelTestExecution {

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
      val address = new InetSocketAddress(InetAddress.getByName("ya.ru"), port)
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

    scenario("Peer blacklist few peers") {

      Given("Peer database is empty")
      val peerDatabase = new PeerDatabaseImpl(TestSettings, None)
      assert(peerDatabase.knownPeers(false).isEmpty)
      assert(peerDatabase.blacklistedPeers().isEmpty)

      When("Peer adds other peers")
      val anotherPeer = new PeerInfo(System.currentTimeMillis)
      val port: Int = 1234
      val address1 = new InetSocketAddress(InetAddress.getByName("bing.com"), port)
      val address2 = new InetSocketAddress(InetAddress.getByName("google.com"), port)
      val address3 = new InetSocketAddress(InetAddress.getByName("yandex.ru"), port)
      peerDatabase.addOrUpdateKnownPeer(address1, anotherPeer)
      peerDatabase.addOrUpdateKnownPeer(address2, anotherPeer)
      peerDatabase.addOrUpdateKnownPeer(address3, anotherPeer)
      assert(!peerDatabase.isBlacklisted(address1))
      assert(!peerDatabase.isBlacklisted(address2))
      assert(!peerDatabase.isBlacklisted(address3))

      And("Peer blacklists other peers")
      peerDatabase.blacklistPeer(address1)
      peerDatabase.blacklistPeer(address2)
      peerDatabase.blacklistPeer(address3)
      assert(peerDatabase.isBlacklisted(address1))
      assert(peerDatabase.isBlacklisted(address2))
      assert(peerDatabase.isBlacklisted(address3))

      And("Peer waits half period")
      Thread.sleep(TestSettings.blacklistResidenceTimeMilliseconds / 2)

      And("Adds one peer to blacklist one more time")
      peerDatabase.blacklistPeer(address2)

      And("Waits another half of period")
      Thread.sleep(TestSettings.blacklistResidenceTimeMilliseconds / 2)

      Then("Two peers disappear from blacklist")
      assert(!peerDatabase.isBlacklisted(address1))
      assert(peerDatabase.isBlacklisted(address2))
      assert(!peerDatabase.isBlacklisted(address3))

      And("Then waits another half of period")
      Thread.sleep(TestSettings.blacklistResidenceTimeMilliseconds / 2)

      And("All peers not in blacklist")
      assert(!peerDatabase.isBlacklisted(address1))
      assert(!peerDatabase.isBlacklisted(address2))
      assert(!peerDatabase.isBlacklisted(address3))
    }
  }

}
