package com.wavesplatform.network

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.{NetworkSettings, loadConfig}
import com.wavesplatform.test.FeatureSpec
import net.ceedubs.ficus.Ficus.*
import org.scalatest.{GivenWhenThen, ParallelTestExecution}

import java.net.{InetAddress, InetSocketAddress}

class BlacklistParallelSpecification extends FeatureSpec with GivenWhenThen with ParallelTestExecution {

  private val config = loadConfig(ConfigFactory.parseString("""waves.network {
                                                              |  known-peers = []
                                                              |  file = null
                                                              |  black-list-residence-time: 1s
                                                              |}""".stripMargin))

  private val networkSettings = config.as[NetworkSettings]("waves.network")

  info("As a Peer")
  info("I want to blacklist other peers for certain time")
  info("So I can give them another chance after")

  Feature("Blacklist") {

    val peerDatabase = new PeerDatabaseImpl(networkSettings)

    val host1    = InetAddress.getByName("1.1.1.1")
    val host2    = InetAddress.getByName("2.2.2.2")
    val host3    = InetAddress.getByName("3.3.3.3")
    val address1 = new InetSocketAddress(host1, 1)
    val address2 = new InetSocketAddress(host2, 2)
    val address3 = new InetSocketAddress(host3, 2)

    def isBlacklisted(address: InetSocketAddress): Boolean =
      peerDatabase.isBlacklisted(address.getAddress)

    Scenario("Peer blacklist another peer") {

      Given("Peer database is empty")
      assert(peerDatabase.knownPeers.isEmpty)
      assert(peerDatabase.detailedBlacklist.isEmpty)

      When("Peer adds another peer to knownPeers")
      peerDatabase.touch(address1)
      assert(peerDatabase.knownPeers.contains(address1))
      assert(!peerDatabase.detailedBlacklist.contains(host1))

      And("Peer blacklists another peer")
      val reason = "because"
      peerDatabase.blacklist(address1.getAddress, reason)
      assert(isBlacklisted(address1))
      assert(peerDatabase.isBlacklisted(host1))
      assert(peerDatabase.detailedBlacklist(host1)._2 == reason)
      assert(!peerDatabase.knownPeers.contains(address1))
      assert(!peerDatabase.knownPeers.contains(address1))

      And("Peer waits for some time")
      Thread.sleep(networkSettings.blackListResidenceTime.toMillis + 500)

      Then("Another peer disappear from blacklist")
      assert(!isBlacklisted(address1))

      And("Another peer became knownPeer")
      assert(peerDatabase.knownPeers.contains(address1))
    }

    Scenario("Peer blacklist few peers") {

      Given("Peer database is empty")
      assert(peerDatabase.knownPeers.isEmpty)
      assert(peerDatabase.detailedBlacklist.isEmpty)

      When("Peer adds other peers")
      peerDatabase.touch(address1)
      peerDatabase.touch(address2)
      peerDatabase.touch(address3)
      assert(!isBlacklisted(address1))
      assert(!isBlacklisted(address2))
      assert(!isBlacklisted(address3))

      And("Peer blacklists other peers")
      peerDatabase.blacklist(address1.getAddress, "")
      peerDatabase.blacklist(address2.getAddress, "")
      peerDatabase.blacklist(address3.getAddress, "")
      assert(isBlacklisted(address1))
      assert(isBlacklisted(address2))
      assert(isBlacklisted(address3))

      And("Peer waits half period")
      Thread.sleep(networkSettings.blackListResidenceTime.toMillis / 2)

      And("Adds one peer to blacklist one more time")
      peerDatabase.blacklist(address2.getAddress, "")

      And("Waits another half of period")
      Thread.sleep((networkSettings.blackListResidenceTime.toMillis / 1.9).toLong)

      Then("Two peers disappear from blacklist")
      assert(!isBlacklisted(address1))
      assert(isBlacklisted(address2))
      assert(!isBlacklisted(address3))

      And("Then waits another half of period")
      Thread.sleep((networkSettings.blackListResidenceTime.toMillis / 1.9).toLong)

      And("All peers not in blacklist")
      assert(!isBlacklisted(address1))
      assert(!isBlacklisted(address2))
      assert(!isBlacklisted(address3))
    }
  }

}
