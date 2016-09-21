package scorex.network

import java.net.{InetAddress, InetSocketAddress}

import com.google.common.primitives.{Ints, Longs}
import scorex.app.ApplicationVersion
import scorex.serialization.{BytesSerializable, Deser}
import scorex.utils.ScorexLogging

import scala.util.Try


case class Handshake(applicationName: String,
                     applicationVersion: ApplicationVersion,
                     nodeName: String,
                     nodeNonce: Long,
                     declaredAddress: Option[InetSocketAddress],
                     time: Long
                    ) extends BytesSerializable {

  require(Option(applicationName).isDefined)
  require(Option(applicationVersion).isDefined)

  lazy val bytes: Array[Byte] = {
    val anb = applicationName.getBytes

    val fab = declaredAddress.map { isa =>
      isa.getAddress.getAddress ++ Ints.toByteArray(isa.getPort)
    }.getOrElse(Array[Byte]())

    val nodeNameBytes = nodeName.getBytes

    Array(anb.size.toByte) ++ anb ++
      applicationVersion.bytes ++
      Array(nodeNameBytes.size.toByte) ++ nodeNameBytes ++
      Longs.toByteArray(nodeNonce) ++
      Ints.toByteArray(fab.length) ++ fab ++
      Longs.toByteArray(time)
  }
}

object Handshake extends ScorexLogging with Deser[Handshake] {
  def parseBytes(bytes: Array[Byte]): Try[Handshake] = Try {
    var position = 0
    val appNameSize = bytes.head
    require(appNameSize > 0, s"Invalid Application name length in handshake: $appNameSize")

    position += 1

    val an = new String(bytes.slice(position, position + appNameSize))
    position += appNameSize

    val av = ApplicationVersion.parseBytes(bytes.slice(position, position + ApplicationVersion.SerializedVersionLength)).get
    position += ApplicationVersion.SerializedVersionLength

    val nodeNameSize = bytes.slice(position, position + 1).head
    position += 1

    val nodeName = new String(bytes.slice(position, position + nodeNameSize))
    position += nodeNameSize

    val nonce = Longs.fromByteArray(bytes.slice(position, position + 8))
    position += 8

    log.trace(s"Incoming handshake: $an $av $nodeName $nonce")

    val fas = Ints.fromByteArray(bytes.slice(position, position + 4))
    position += 4

    val isaOpt = if (fas > 0) {
      val fa = bytes.slice(position, position + fas - 4)
      position += fas - 4

      val port = Ints.fromByteArray(bytes.slice(position, position + 4))
      position += 4

      Some(new InetSocketAddress(InetAddress.getByAddress(fa), port))
    } else None

    val time = Longs.fromByteArray(bytes.slice(position, position + 8))

    Handshake(an, av, nodeName, nonce, isaOpt, time)
  }
}