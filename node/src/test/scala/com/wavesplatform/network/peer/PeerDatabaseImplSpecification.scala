package com.wavesplatform.network.peer

import com.google.common.base.Ticker
import com.typesafe.config.ConfigFactory
import com.wavesplatform.network.{PeerDatabase, PeerDatabaseImpl}
import com.wavesplatform.settings.NetworkSettings
import com.wavesplatform.test.FreeSpec
import net.ceedubs.ficus.Ficus.*

import java.net.InetSocketAddress
import scala.concurrent.duration.*

class PeerDatabaseImplSpecification extends FreeSpec {

  val host1    = "1.1.1.1"
  val host2    = "2.2.2.2"
  val address1 = new InetSocketAddress(host1, 1)
  val address2 = new InetSocketAddress(host2, 2)

  private val config1 = ConfigFactory
    .parseString("""waves.network {
                   |  file = null
                   |  known-peers = []
                   |  peers-data-residence-time: 2s
                   |}""".stripMargin)
    .withFallback(ConfigFactory.load())
    .resolve()
  private val settings1 = config1.as[NetworkSettings]("waves.network")

  private val config2 = ConfigFactory
    .parseString("""waves.network {
                   |  file = null
                   |  known-peers = []
                   |  peers-data-residence-time = 10s
                   |}""".stripMargin)
    .withFallback(ConfigFactory.load())
    .resolve()
  private val settings2 = config2.as[NetworkSettings]("waves.network")

  private var ts                 = 0L
  private def sleepLong(): Unit  = { ts += 2200.millis.toNanos }
  private def sleepShort(): Unit = { ts += 200.millis.toNanos }
  private def withDatabase(settings: NetworkSettings)(f: PeerDatabase => Unit): Unit = {
    val pdb = new PeerDatabaseImpl(
      settings,
      new Ticker {
        override def read(): Long = ts
      }
    )
    f(pdb)
    pdb.close()
  }

  "Peer database" - {
    "new candidate should be returned by randomPeer, but should not be added to knownPeers" in withDatabase(settings1) { database =>
      database.knownPeers shouldBe empty
      database.addCandidate(address1)
      database.nextCandidate(Set()) should contain(address1)
      database.knownPeers shouldBe empty
    }

    "touch() should add the address to knownPeers" in withDatabase(settings1) { database =>
      database.knownPeers shouldBe empty
      database.addCandidate(address1)
      database.knownPeers shouldBe empty
      database.touch(address1)
      database.knownPeers.keys should contain(address1)
    }

    "peer should should become obsolete after time" in withDatabase(settings1) { database =>
      database.touch(address1)
      database.knownPeers.keys should contain(address1)
      sleepLong()
      database.knownPeers shouldBe empty
      database.nextCandidate(Set()) shouldBe empty
    }

    "touching peer prevent it from obsoleting" in withDatabase(settings1) { database =>
      database.addCandidate(address1)
      database.touch(address1)
      sleepLong()
      database.touch(address1)
      sleepShort()
      database.knownPeers.keys should contain(address1)
    }

    "blacklisted peer should disappear from internal buffer and database" in withDatabase(settings1) { database =>
      database.touch(address1)
      database.addCandidate(address2)
      database.knownPeers.keys should contain(address1)
      database.knownPeers.keys should not contain address2

      database.blacklist(address1.getAddress, "")
      database.knownPeers.keys should not contain address1
      database.knownPeers should be(empty)

      database.nextCandidate(Set()) should contain(address2)
      database.blacklist(address2.getAddress, "")
      database.nextCandidate(Set()) should not contain address2
      database.nextCandidate(Set()) should be(empty)
    }

    "random peer should return peers from both from database and buffer" in withDatabase(settings2) { database2 =>
      database2.touch(address1)
      database2.addCandidate(address2)
      val keys = database2.knownPeers.keys
      keys should contain(address1)
      keys should not contain address2

      val set = (1 to 10).flatMap(i => database2.nextCandidate(Set())).toSet

      set should contain(address1)
      set should contain(address2)
    }

    "filters out excluded candidates" in withDatabase(settings1) { database =>
      database.addCandidate(address1)
      database.addCandidate(address1)
      database.addCandidate(address2)

      database.nextCandidate(Set(address1)) should contain(address2)
    }

    "filters out wildcard addresses" in withDatabase(settings1) { database =>
      database.addCandidate(new InetSocketAddress("0.0.0.0", 6863))
      database.nextCandidate(Set(address1, address2)) shouldBe None
    }

    "should not add nodes to the blacklist if blacklisting is disabled" in {
      val config = ConfigFactory
        .parseString(s"""waves.network {
                        |  file = null
                        |  known-peers = []
                        |  peers-data-residence-time = 100s
                        |  enable-blacklisting = no
                        |}""".stripMargin)
        .withFallback(ConfigFactory.load())
        .resolve()
      val settings = config.as[NetworkSettings]("waves.network")
      val database = new PeerDatabaseImpl(settings)
      database.blacklist(address1.getAddress, "I don't like it")

      database.detailedBlacklist shouldBe empty
    }
  }

}
