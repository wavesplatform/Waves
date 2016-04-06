package scorex.network

import java.net.{InetAddress, InetSocketAddress}

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import play.api.libs.json.{JsObject, Json}
import scorex.network.peer.{PeerDatabaseImpl, PeerInfo}
import scorex.settings.Settings


class PeerDatabaseSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks
  with Matchers {

  object TestSettings extends Settings {
    override lazy val settingsJSON: JsObject = Json.obj()
    override val filename: String = ""
  }

  val db = new PeerDatabaseImpl(TestSettings, None)
  val pi = PeerInfo(System.currentTimeMillis(), None, None)
  val portGen = Gen.choose(1, 0xFFFF)
  val addGen = for {
    a1: Int <- Gen.choose(1, 255)
    a2: Int <- Gen.choose(1, 255)
    a3: Int <- Gen.choose(1, 255)
    a4: Int <- Gen.choose(1, 255)
  } yield s"$a1.$a2.$a3.$a4"


  property("peer blacklisting") {
    forAll(addGen, portGen) { (add: String, port: Int) =>
      val address = new InetSocketAddress(InetAddress.getByName(add), port)
      db.addOrUpdateKnownPeer(address, pi)
      db.knownPeers(false).contains(address) shouldBe true

      db.blacklistPeer(address)
      db.knownPeers(false).contains(address) shouldBe false
      db.blacklistedPeers().contains(address.getHostName) shouldBe true
    }
  }
}