package scorex.utils

import play.api.libs.json._
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success}

trait JsonSerialization {

  type Bytes = Array[Byte]

  implicit val bytesWrites = new Writes[Bytes] {
    def writes(bytes: Bytes) = JsString(Base58.encode(bytes))
  }

  implicit def bytesReads: Reads[Bytes] = new Reads[Bytes] {
    def reads(json: JsValue): JsResult[Bytes] = json match {
      case JsString(encoded) =>
        Base58.decode(encoded) match {
          case Success(decoded) =>
            JsSuccess(decoded)
          case Failure(e) =>
            throw new RuntimeException(s"Failed to parse Base58 encoded bytes")
        }
      case m =>
        throw new RuntimeException(s"Bigint MUST be represented as string in json $m ${m.getClass} given")
    }
  }

  implicit val bigIntWrites = new Writes[BigInt] {
    def writes(bitInt: BigInt) = JsString(bitInt.toString)
  }

  implicit def bigIntReads: Reads[BigInt] = new Reads[BigInt] {
    def reads(json: JsValue): JsResult[BigInt] = json match {
      case JsString(bigint) =>
        JsSuccess(BigInt(bigint))
      case JsNumber(bigint) =>
        JsSuccess(BigInt(bigint.toString))
      case m =>
        throw new RuntimeException(s"Bigint MUST be represented as string in json $m ${m.getClass} given")
    }
  }
}
