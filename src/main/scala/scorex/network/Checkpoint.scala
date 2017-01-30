package scorex.network

import com.google.common.primitives.{Bytes, Ints}
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json._
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import play.api.libs.functional.syntax._

import scala.collection.immutable.Stream
import scala.util.{Failure, Success}

@SerialVersionUID(-4750343171084108636L)
case class BlockCheckpoint(height: Int,
                           @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte])

@SerialVersionUID(-3551519894804767122L)
case class Checkpoint(items: Seq[BlockCheckpoint],
                      @ApiModelProperty(dataType = "java.lang.String")signature: Array[Byte]) {
  def toSign: Array[Byte] = {
    val length = items.size
    val lengthBytes = Ints.toByteArray(length)

    items.foldLeft(lengthBytes) { case (bs, BlockCheckpoint(h, s)) =>
      Bytes.concat(bs, Ints.toByteArray(h), s)
    }
  }

  def signedBy(privateKey: Array[Byte]) = copy(signature = EllipticCurveImpl.sign(privateKey, toSign))
}

object Checkpoint {
  def historyPoints(n: Int, maxRollback: Int, resultSize: Int = MaxCheckpoints): Seq[Int] =
    mult(maxRollback, 2).map(n - _).takeWhile(_ > 0).take(resultSize)

  private def mult(start: Int, step: Int): Stream[Int] =
    Stream.cons(start, mult(start * step, step))

  val MaxCheckpoints = 10

  implicit val byteArrayReads = new Reads[Array[Byte]] {
    def reads(json: JsValue) = json match {
      case JsString(s) => Base58.decode(s) match {
        case Success(bytes) if bytes.length == scorex.transaction.TypedTransaction.SignatureLength => JsSuccess(bytes)
        case Success(bytes) => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.incorrect.signatureLength"))))
        case Failure(_) => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.incorrect.base58"))))
      }
      case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.jsstring"))))
    }
  }

  implicit val blockCheckpoint: Reads[BlockCheckpoint] = {
    val r = (JsPath \ "height").read[Int] and
      (JsPath \ "signature").read[Array[Byte]]
    r(BlockCheckpoint.apply _)
  }

  implicit val checkpointReads: Reads[Checkpoint] = {
    val r = (JsPath \ "items").read[Seq[BlockCheckpoint]] and
      (JsPath \ "signature").read[Array[Byte]]
    r(Checkpoint.apply _)
  }

}


