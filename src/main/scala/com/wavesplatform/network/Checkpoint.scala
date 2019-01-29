package com.wavesplatform.network

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.common.utils.Base58
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json._
import com.wavesplatform.crypto._

import scala.collection.immutable.Stream
import scala.util.{Failure, Success}

case class BlockCheckpoint(height: Int, @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte])

case class Checkpoint(items: Seq[BlockCheckpoint], @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte]) {
  def toSign: Array[Byte] = {
    val length      = items.size
    val lengthBytes = Ints.toByteArray(length)

    items.foldLeft(lengthBytes) {
      case (bs, BlockCheckpoint(h, s)) =>
        Bytes.concat(bs, Ints.toByteArray(h), s)
    }
  }
}

object Checkpoint {
  def historyPoints(n: Int, maxRollback: Int, resultSize: Int = MaxCheckpoints): Seq[Int] =
    mult(maxRollback, 2).map(n - _).takeWhile(_ > 0).take(resultSize)

  private def mult(start: Int, step: Int): Stream[Int] =
    Stream.cons(start, mult(start * step, step))

  val MaxCheckpoints = 10

  implicit val byteArrayReads = new Reads[Array[Byte]] {
    def reads(json: JsValue) = json match {
      case JsString(s) =>
        Base58.decode(s) match {
          case Success(bytes) if bytes.length == SignatureLength => JsSuccess(bytes)
          case Success(bytes)                                    => JsError(JsonValidationError("error.incorrect.signatureLength", bytes.length.toString))
          case Failure(t)                                        => JsError(JsonValidationError(Seq("error.incorrect.base58", t.getLocalizedMessage), s))
        }
      case _ => JsError("error.expected.jsstring")
    }
  }

  implicit val blockCheckpointFormat: Reads[BlockCheckpoint] = Json.reads
  implicit val checkpointFormat: Reads[Checkpoint]           = Json.reads
}
