package scorex.network

import java.net.{InetAddress, InetSocketAddress}

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.NetworkSettings
import org.scalatest.{FeatureSpec, GivenWhenThen}
import scorex.network.peer.PeerDatabaseImpl

class BlacklistSpecification extends FeatureSpec with GivenWhenThen {
  private val config = ConfigFactory.parseString(
    """
      |waves {
      |  network {
      |    black-list-residence-time: 1s
      |  }
      |}
    """.stripMargin).withFallback(ConfigFactory.load()).resolve()

  private val networkSettings = NetworkSettings.fromConfig(config)

  info("As a Peer")
  info("I want to blacklist other peers for certain time")
  info("So I can give them another chance after")

  feature("Blacklist") {
    scenario("Peer blacklist another peer") {

      Given("Peer database is empty")
      val peerDatabase = new PeerDatabaseImpl(networkSettings, None)

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
      Thread.sleep(networkSettings.blackListResidenceTime.toMillis)

      Then("Another peer disappear from blacklist")
      assert(!isBlacklisted(address))

      And("Another peer became known")
      assert(peerDatabase.getKnownPeers.contains(address))
    }
  }
}
