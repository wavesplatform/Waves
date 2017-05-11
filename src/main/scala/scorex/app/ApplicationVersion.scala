package scorex.app

import com.google.common.primitives.Ints
import io.netty.buffer.ByteBuf
import scorex.serialization.BytesSerializable

import scala.util.Try

case class ApplicationVersion(firstDigit: Int, secondDigit: Int, thirdDigit: Int) extends BytesSerializable {
  def this(v: (Int, Int, Int)) = this(v._1, v._2, v._3)

  lazy val bytes: Array[Byte] = Ints.toByteArray(firstDigit) ++ Ints.toByteArray(secondDigit) ++ Ints.toByteArray(thirdDigit)

  def compatibleWith(other: ApplicationVersion): Boolean =
    firstDigit == other.firstDigit && Math.abs(secondDigit - other.secondDigit) < 2

  override def toString: String = s"$firstDigit.$secondDigit.$thirdDigit"
}

object ApplicationVersion {
  val SerializedVersionLength: Int = 4 * 3

  def parseByteBuf(bytes: ByteBuf) = ApplicationVersion(bytes.readInt(), bytes.readInt(), bytes.readInt())

  def parseBytes(bytes: Array[Byte]): Try[ApplicationVersion] = Try {
    require(bytes.length == SerializedVersionLength, "Wrong bytes for application version")
    ApplicationVersion(
      Ints.fromByteArray(bytes.slice(0, 4)),
      Ints.fromByteArray(bytes.slice(4, 8)),
      Ints.fromByteArray(bytes.slice(8, 12))
    )
  }
}
