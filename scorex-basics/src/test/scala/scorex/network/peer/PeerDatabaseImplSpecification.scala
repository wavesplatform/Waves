package scorex.network.peer

import java.net.InetSocketAddress

import org.scalatest.Matchers
import scorex.settings.SettingsMock

import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.{implicitConversions, postfixOps}

class PeerDatabaseImplSpecification extends org.scalatest.path.FreeSpecLike with Matchers {

  object TestSettings extends SettingsMock {
    override lazy val dataDirOpt: Option[String] = None
    override lazy val peersDataResidenceTime: FiniteDuration = 10 seconds
  }

  val database = new PeerDatabaseImpl(TestSettings, None)

  def peers = database.knownPeers(false)

  val addr = new InetSocketAddress("localhost", 111)

  "obsolete peers data cleanup" - {

    peers shouldBe empty

    database.mergePeerInfo(addr, PeerInfo())

    "peer in the database" in {
      peers.keys should contain(addr)
    }

    "residence time" in {
      database.mergePeerInfo(addr, PeerInfo(System.currentTimeMillis() + 1000 * 60), createIfNotExists = false)

      peers.keys should contain(addr)

      database.mergePeerInfo(
        addr,
        PeerInfo(System.currentTimeMillis() - TestSettings.peersDataResidenceTime.toMillis),
        createIfNotExists = false)

      peers shouldBe empty
    }
  }

  "set current time as last seen time for new records" in {
    database.mergePeerInfo(addr, PeerInfo(lastSeen = 1000))

    assert(peers(addr).lastSeen != 1000)
  }

  "leave last seen unchanged if not set" in {
    database.mergePeerInfo(addr, PeerInfo())

    val before = peers(addr).lastSeen

    Thread sleep 100

    database.mergePeerInfo(addr, PeerInfo())

    peers(addr).lastSeen shouldBe before
  }
}
