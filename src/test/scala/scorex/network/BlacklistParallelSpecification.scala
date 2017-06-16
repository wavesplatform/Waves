package scorex.network

import java.net.{InetAddress, InetSocketAddress}

import com.typesafe.config.ConfigFactory
import com.wavesplatform.network.PeerDatabaseImpl
import com.wavesplatform.settings.{NetworkSettings, loadConfig}
import net.ceedubs.ficus.Ficus._
import org.scalatest.{FeatureSpec, GivenWhenThen, ParallelTestExecution}

class BlacklistParallelSpecification extends FeatureSpec with GivenWhenThen with ParallelTestExecution {

  private val config = loadConfig(ConfigFactory.parseString(
    """waves.network {
      |  file = null
      |  black-list-residence-time: 1s
      |}""".stripMargin))

  private val networkSettings = config.as[NetworkSettings]("waves.network")

  info("As a Peer")
  info("I want to blacklist other peers for certain time")
  info("So I can give them another chance after")

  feature("Blacklist") {

    val peerDatabase = new PeerDatabaseImpl(networkSettings)

    val host1 = InetAddress.getByName("1.1.1.1")
    val host2 = InetAddress.getByName("2.2.2.2")
    val host3 = InetAddress.getByName("3.3.3.3")
    val address1 = new InetSocketAddress(host1, 1)
    val address2 = new InetSocketAddress(host2, 2)
    val address3 = new InetSocketAddress(host3, 2)

    def isBlacklisted(address: InetSocketAddress): Boolean =
      peerDatabase.getBlacklist.contains(address.getAddress)

    scenario("Peer blacklist another peer") {

      Given("Peer database is empty")
      assert(peerDatabase.getKnownPeers.isEmpty)
      assert(peerDatabase.getBlacklist.isEmpty)

      When("Peer adds another peer to knownPeers")
      peerDatabase.addPeer(address1, Some(0), None)
      assert(peerDatabase.getKnownPeers.contains(address1))
      assert(!peerDatabase.getBlacklist.contains(host1))

      And("Peer blacklists another peer")
      peerDatabase.blacklistHost(host1)
      assert(isBlacklisted(address1))
      assert(peerDatabase.getBlacklist.contains(host1))
      assert(!peerDatabase.getKnownPeers.contains(address1))

      And("Peer waits for some time")
      Thread.sleep(networkSettings.blackListResidenceTime.toMillis)

      Then("Another peer disappear from blacklist")
      assert(!isBlacklisted(address1))

      And("Another peer became knownPeer")
      assert(peerDatabase.getKnownPeers.contains(address1))
    }

    scenario("Peer blacklist few peers") {

      Given("Peer database is empty")
      assert(peerDatabase.getKnownPeers.isEmpty)
      assert(peerDatabase.getBlacklist.isEmpty)

      When("Peer adds other peers")
      peerDatabase.addPeer(address1, Some(0), None)
      peerDatabase.addPeer(address2, Some(1), None)
      peerDatabase.addPeer(address3, Some(2), None)
      assert(!isBlacklisted(address1))
      assert(!isBlacklisted(address2))
      assert(!isBlacklisted(address3))

      And("Peer blacklists other peers")
      peerDatabase.blacklistHost(address1.getAddress)
      peerDatabase.blacklistHost(address2.getAddress)
      peerDatabase.blacklistHost(address3.getAddress)
      assert(isBlacklisted(address1))
      assert(isBlacklisted(address2))
      assert(isBlacklisted(address3))

      And("Peer waits half period")
      Thread.sleep(networkSettings.blackListResidenceTime.toMillis / 2)

      And("Adds one peer to blacklist one more time")
      peerDatabase.blacklistHost(address2.getAddress)

      And("Waits another half of period")
      Thread.sleep(networkSettings.blackListResidenceTime.toMillis / 2)

      Then("Two peers disappear from blacklist")
      assert(!isBlacklisted(address1))
      assert(isBlacklisted(address2))
      assert(!isBlacklisted(address3))

      And("Then waits another half of period")
      Thread.sleep(networkSettings.blackListResidenceTime.toMillis / 2)

      And("All peers not in blacklist")
      assert(!isBlacklisted(address1))
      assert(!isBlacklisted(address2))
      assert(!isBlacklisted(address3))
    }
  }

}
