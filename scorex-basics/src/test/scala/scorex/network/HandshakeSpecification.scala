package scorex.network

import java.net.{InetAddress, InetSocketAddress}

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.app.ApplicationVersion


class HandshakeSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  val MaxVersion = 999
  val MaxIp = 255
  val MaxPort = 65535

  val appVersionGen = for {
    fd <- Gen.choose(0, MaxVersion)
    sd <- Gen.choose(0, MaxVersion)
    td <- Gen.choose(0, MaxVersion)
  } yield ApplicationVersion(fd, sd, td)

  val isGen = for {
    ip1 <- Gen.choose(0, MaxIp)
    ip2 <- Gen.choose(0, MaxIp)
    ip3 <- Gen.choose(0, MaxIp)
    ip4 <- Gen.choose(0, MaxIp)
    port <- Gen.choose(0, MaxPort)
  } yield new InetSocketAddress(InetAddress.getByName(s"$ip1.$ip2.$ip3.$ip4"), port)

  val validNumers =
    for (n <- Gen.choose(Integer.MIN_VALUE + 1, Integer.MAX_VALUE)) yield n

  property("handshake should remain the same after serialization/deserialization") {
    forAll(Gen.alphaStr, appVersionGen, isGen, Gen.posNum[Long], Gen.posNum[Long]) {
      (appName: String,
       av: ApplicationVersion,
       isa: InetSocketAddress,
       nonce: Long,
       time: Long) =>

        val h1 = Handshake(appName, av, None, nonce, time)

        val hr1 = Handshake.parse(h1.bytes).get

        hr1.applicationName should be(h1.applicationName)
        hr1.applicationVersion should be(h1.applicationVersion)
        hr1.fromAddress should be(h1.fromAddress)
        hr1.fromNonce should be(h1.fromNonce)
        hr1.time should be(h1.time)


        val h2 = Handshake(appName, av, Some(isa), nonce, time)

        val hr2 = Handshake.parse(h2.bytes).get

        hr2.applicationName should be(h2.applicationName)
        hr2.applicationVersion should be(h2.applicationVersion)
        hr2.fromAddress should be(h2.fromAddress)
        hr2.fromNonce should be(h2.fromNonce)
        hr2.time should be(h2.time)
    }
  }
}