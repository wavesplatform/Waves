package com.wavesplatform.network.peer

import java.io.File
import java.net.InetSocketAddress
import java.nio.file.Files

import com.typesafe.config.ConfigFactory
import com.wavesplatform.network.{PeerDatabase, PeerDatabaseImpl}
import com.wavesplatform.settings.NetworkSettings
import com.wavesplatform.test.FreeSpec
import net.ceedubs.ficus.Ficus.*

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

  private def withDatabase(settings: NetworkSettings)(f: PeerDatabase => Unit): Unit = {
    val pdb = new PeerDatabaseImpl(settings)
    f(pdb)
    pdb.close()
  }

  "Peer database" - {
    "new peer should not appear in internal buffer but does not appear in database" in withDatabase(settings1) { database =>
      database.knownPeers shouldBe empty
      database.addCandidate(address1)
      database.randomPeer(Set()) should contain(address1)
      database.knownPeers shouldBe empty
    }

    "new peer should move from internal buffer to database" in withDatabase(settings1) { database =>
      database.knownPeers shouldBe empty
      database.addCandidate(address1)
      database.knownPeers shouldBe empty
      database.touch(address1)
      database.knownPeers.keys should contain(address1)
    }

    "peer should should became obsolete after time" in withDatabase(settings1) { database =>
      database.touch(address1)
      database.knownPeers.keys should contain(address1)
      sleepLong()
      database.knownPeers shouldBe empty
      database.randomPeer(Set()) shouldBe empty
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

      database.randomPeer(Set()) should contain(address2)
      database.blacklist(address2.getAddress, "")
      database.randomPeer(Set()) should not contain address2
      database.randomPeer(Set()) should be(empty)
    }

    "random peer should return peers from both from database and buffer" in withDatabase(settings2) { database2 =>
      database2.touch(address1)
      database2.addCandidate(address2)
      val keys = database2.knownPeers.keys
      keys should contain(address1)
      keys should not contain address2

      val set = (1 to 10).flatMap(i => database2.randomPeer(Set())).toSet

      set should contain(address1)
      set should contain(address2)
    }

    "filters out excluded candidates" in withDatabase(settings1) { database =>
      database.addCandidate(address1)
      database.addCandidate(address1)
      database.addCandidate(address2)

      database.randomPeer(Set(address1)) should contain(address2)
    }

    "filters out wildcard addresses" in withDatabase(settings1) { database =>
      database.addCandidate(new InetSocketAddress("0.0.0.0", 6863))
      database.randomPeer(Set(address1, address2)) shouldBe None
    }

    "if blacklisting is disabled" - {
      "should clear blacklist at start" in {
        val databaseFile = Files.createTempFile("waves-tests", "PeerDatabaseImplSpecification-blacklisting-clear").toAbsolutePath.toString
        val path         = if (File.separatorChar == '\\') databaseFile.replace('\\', '/') else databaseFile
        val prevConfig = ConfigFactory
          .parseString(s"""waves.network {
                          |  file = "$path"
                          |  known-peers = []
                          |  peers-data-residence-time = 100s
                          |}""".stripMargin)
          .withFallback(ConfigFactory.load())
          .resolve()
        val prevSettings = prevConfig.as[NetworkSettings]("waves.network")
        val prevDatabase = new PeerDatabaseImpl(prevSettings)
        prevDatabase.blacklist(address1.getAddress, "I don't like it")
        prevDatabase.close()

        val config = ConfigFactory
          .parseString(s"""waves.network {
                          |  file = "$path"
                          |  known-peers = []
                          |  peers-data-residence-time = 100s
                          |  enable-blacklisting = no
                          |}""".stripMargin)
          .withFallback(ConfigFactory.load())
          .resolve()
        val settings = config.as[NetworkSettings]("waves.network")
        val database = new PeerDatabaseImpl(settings)

        database.blacklistedHosts shouldBe empty
      }

      "should not add nodes to the blacklist" in {
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

        database.blacklistedHosts shouldBe empty
      }
    }
  }

  private def sleepLong() = Thread.sleep(2200)

  private def sleepShort() = Thread.sleep(200)

}
