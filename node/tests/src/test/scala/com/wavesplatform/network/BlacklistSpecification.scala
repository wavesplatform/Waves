package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.NetworkSettings
import com.wavesplatform.test.FeatureSpec
import net.ceedubs.ficus.Ficus._
import org.scalatest.GivenWhenThen

class BlacklistSpecification extends FeatureSpec with GivenWhenThen {
  private val config = ConfigFactory.parseString("""waves.network {
      |  known-peers = []
      |  file = null
      |  black-list-residence-time: 1s
      |}""".stripMargin).withFallback(ConfigFactory.load()).resolve()

  private val networkSettings = config.as[NetworkSettings]("waves.network")

  info("As a Peer")
  info("I want to blacklist other peers for certain time")
  info("So I can give them another chance after")

  Feature("Blacklist") {
    Scenario("Peer blacklist another peer") {

      Given("Peer database is empty")
      val peerDatabase = new PeerDatabaseImpl(networkSettings)

      def isBlacklisted(address: InetSocketAddress) = peerDatabase.blacklistedHosts.contains(address.getAddress)

      assert(peerDatabase.knownPeers.isEmpty)
      assert(peerDatabase.blacklistedHosts.isEmpty)

      When("Peer adds another peer to knownPeers")
      val address = new InetSocketAddress(InetAddress.getByName("localhost"), 1234)
      peerDatabase.touch(address)
      assert(peerDatabase.knownPeers.contains(address))
      assert(!isBlacklisted(address))

      And("Peer blacklists another peer")
      peerDatabase.blacklist(address.getAddress, "")
      assert(isBlacklisted(address))
      assert(!peerDatabase.knownPeers.contains(address))

      And("Peer waits for some time")
      Thread.sleep(networkSettings.blackListResidenceTime.toMillis + 500)

      Then("Another peer disappear from blacklist")
      assert(!isBlacklisted(address))

      And("Another peer became known")
      assert(peerDatabase.knownPeers.contains(address))
    }
  }
}
