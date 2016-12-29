package scorex.network.peer

import java.net.InetSocketAddress

import org.scalatest.{Matchers, path}
import scorex.settings.SettingsMock

import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.{implicitConversions, postfixOps}

class PeerDatabaseImplSpecification extends path.FreeSpecLike with Matchers {

  val database = new PeerDatabaseImpl(TestSettings, None)
  val host1 = "1.1.1.1"
  val host2 = "2.2.2.2"
  val address1 = new InetSocketAddress(host1, 1)
  val address2 = new InetSocketAddress(host2, 2)

  "Peer database" - {
    "new peer should not appear in internal buffer but does not appear in database" in {
      database.getKnownPeers shouldBe empty
      database.addPeer(address1, None, None)
      database.getRandomPeer(Set()) should contain(address1)
      database.getKnownPeers shouldBe empty
    }

    "new peer should move from internal buffer to database" in {
      database.getKnownPeers shouldBe empty
      database.addPeer(address1, None, None)
      database.getKnownPeers shouldBe empty
      database.addPeer(address1, Some(0), None)
      database.getKnownPeers.keys should contain(address1)
    }

    "peer should should became obsolete after time" in {
      database.addPeer(address1, Some(0), None)
      database.getKnownPeers.keys should contain(address1)
      sleepLong()
      database.getKnownPeers shouldBe empty
      database.getRandomPeer(Set()) shouldBe empty
    }

    "touching peer prevent it from obsoleting" in {
      database.addPeer(address1, None, None)
      database.addPeer(address1, Some(0), None)
      sleepLong()
      database.touch(address1)
      sleepShort()
      database.getKnownPeers.keys should contain(address1)
    }

    "removing peer removes it from internal buffer" in {
      database.addPeer(address1, None, None)
      database.getRandomPeer(Set()) should contain(address1)
      database.removePeer(address1)
      database.getRandomPeer(Set()) shouldBe empty
    }

    "removing peer removes it also from database" in {
      database.addPeer(address1, Some(0), None)
      database.getKnownPeers.keys should contain(address1)
      database.removePeer(address1)
      database.getKnownPeers.keys shouldBe empty
    }

    "random peer should return peers from both from database and buffer" in {
      database.addPeer(address1, Some(0), None)
      database.addPeer(address2, None, None)
      database.getKnownPeers.keys should contain(address1)
      database.getKnownPeers.keys should not contain (address2)

      val set = scala.collection.mutable.Set.empty[InetSocketAddress]

      (1 to 10).foreach(i => database.getRandomPeer(Set()).foreach(a => set.add(a)))

      set should contain(address1)
      set should contain(address2)
    }

    "blacklisted peer should disappear from internal buffer and database" in {
      database.addPeer(address1, Some(0), None)
      database.addPeer(address2, None, None)
      database.getKnownPeers.keys should contain(address1)
      database.getKnownPeers.keys should not contain (address2)

      database.blacklistHost(host1)
      database.getKnownPeers.keys should not contain (address1)
      database.getKnownPeers shouldBe empty

      database.getRandomPeer(Set()) should contain(address2)
      database.blacklistHost(host2)
      database.getRandomPeer(Set()) should not contain (address2)
      database.getRandomPeer(Set()) shouldBe empty
    }

  }

  private def sleepLong() = Thread.sleep(1200)

  private def sleepShort() = Thread.sleep(600)

  object TestSettings extends SettingsMock {
    override lazy val dataDirOpt: Option[String] = None
    override lazy val peersDataResidenceTime: FiniteDuration = 1 seconds
  }

}
