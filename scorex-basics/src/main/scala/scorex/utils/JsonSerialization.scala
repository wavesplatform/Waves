package scorex.utils

import play.api.libs.json._

trait JsonSerialization {

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
