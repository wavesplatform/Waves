package scorex.network

import java.net.{InetAddress, InetSocketAddress}

import com.google.common.primitives.{Ints, Longs}
import scorex.app.ApplicationVersion
import scorex.utils.ScorexLogging

import scala.util.{Failure, Try}


case class Handshake(applicationName: String,
                     applicationVersion: ApplicationVersion,
                     fromAddress: Option[InetSocketAddress],
                     fromNonce: Long,
                     time: Long
                    ) {

  lazy val bytes: Array[Byte] = {
    val anb = applicationName.getBytes

    val fab = fromAddress.map { isa =>
      isa.getAddress.getAddress ++ Ints.toByteArray(isa.getPort)
    }.getOrElse(Array[Byte]())

    Array(anb.size.toByte) ++ anb ++
      applicationVersion.bytes ++
      Ints.toByteArray(fab.length) ++ fab ++
      Longs.toByteArray(fromNonce) ++
      Longs.toByteArray(time)
  }
}

object Handshake extends ScorexLogging {
  def parse(bytes: Array[Byte]): Try[Handshake] = Try {
    var position = 0
    val appNameSize = bytes.head
    position += 1

    val an = new String(bytes.slice(position, position + appNameSize))
    position += appNameSize

    val av = ApplicationVersion.parse(bytes.slice(position, position + ApplicationVersion.SerializedVersionLength)).get
    position += ApplicationVersion.SerializedVersionLength

    val fas = Ints.fromByteArray(bytes.slice(position, position + 4))
    position += 4

    val isaOpt = if(fas>0){
      val fa = bytes.slice(position, position + fas - 4)
      position += fas - 4

      val port = Ints.fromByteArray(bytes.slice(position, position + 4))
      position += 4

      Some(new InetSocketAddress(InetAddress.getByAddress(fa), port))
    } else None

    val nonce = Longs.fromByteArray(bytes.slice(position, position + 8))
    position += 8

    val time = Longs.fromByteArray(bytes.slice(position, position + 8))

    Handshake(an, av, isaOpt, nonce, time)
  }.recoverWith { case t: Throwable =>
    log.info("Error during handshake parsing:", t)
    Failure(t)
  }
}