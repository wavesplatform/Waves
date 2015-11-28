package scorex.perma.consensus

import play.api.libs.functional.syntax._
import play.api.libs.json._

case class PermaLikeConsensusBlockData(target: BigInt, puz: Array[Byte], ticket: Ticket)

object PermaLikeConsensusBlockData {
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

  implicit val writes: Writes[PermaLikeConsensusBlockData] = (
    (JsPath \ "difficulty").write[BigInt] and
      (JsPath \ "puz").write[Array[Byte]] and
      (JsPath \ "ticket").write[Ticket]
    ) (unlift(PermaLikeConsensusBlockData.unapply))

  implicit val reads: Reads[PermaLikeConsensusBlockData] = (
    (JsPath \ "difficulty").read[BigInt] and
      (JsPath \ "puz").read[Array[Byte]] and
      (JsPath \ "ticket").read[Ticket]
    ) (PermaLikeConsensusBlockData.apply _)

}