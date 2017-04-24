package scorex.app

import com.google.common.primitives.Ints
import scorex.serialization.BytesSerializable

import scala.util.Try

case class ApplicationVersion(firstDigit: Int, secondDigit: Int, thirdDigit: Int) extends BytesSerializable {
  def this(v: (Int, Int, Int)) = this(v._1, v._2, v._3)

  lazy val bytes: Array[Byte] = Ints.toByteArray(firstDigit) ++ Ints.toByteArray(secondDigit) ++ Ints.toByteArray(thirdDigit)
}

object ApplicationVersion {
  val SerializedVersionLength: Int = 4 * 3

  def parseBytes(bytes: Array[Byte]): Try[ApplicationVersion] = Try {
    require(bytes.length == SerializedVersionLength, "Wrong bytes for application version")
    ApplicationVersion(
      Ints.fromByteArray(bytes.slice(0, 4)),
      Ints.fromByteArray(bytes.slice(4, 8)),
      Ints.fromByteArray(bytes.slice(8, 12))
    )
  }
}
