package com.wavesplatform.lang.v1.repl.node.http.response.model

import io.circe.{Decoder, DecodingFailure, HCursor}

sealed trait Attachment
case object ANothing extends Attachment
case class ABytes(v: ByteString) extends Attachment
case class AStr(v: String) extends Attachment
case class AInt(v: Long) extends Attachment
case class ABoolean(v: Boolean) extends Attachment
object ATrue extends ABoolean(true)
object AFalse extends ABoolean(false)

object Attachment {
  implicit val adecoder: Decoder[Attachment] = (c:HCursor) =>
    for {
      t <- c.downField("type").as[String]
      v = c.downField("value")
      result <- t match {
        case "integer" => v.as[Long].map(AInt.apply)
        case "boolean" => v.as[Boolean].map { case true => ATrue ; case false => AFalse }
        case "binary" => v.as[ByteString].map(ABytes.apply)
        case "string" => v.as[String].map(AStr.apply)
        case v => Left(DecodingFailure.apply(s"Attachment type not supported: $v", List()))
      }
    } yield result
}
import Attachment._

private[node] case class TransferTransaction(
  id: ByteString,
  recipient: ByteString,
  amount: Long,
  assetId: Option[ByteString],
  feeAssetId: Option[ByteString],
  attachment: Attachment,
  fee: Long,
  timestamp: Long,
  height: Int,
  `type`: Byte,
  version: Byte,
  senderPublicKey: ByteString,
  proofs: List[ByteString],
  bodyBytes: ByteString
)

object Transaction {
  implicit val decoder: Decoder[TransferTransaction] = (c: HCursor) =>
   for {
     applicationStatus <- c.downField("applicationStatus").as[Option[String]]
     succeed = applicationStatus.fold(true)(_ == "succeed")
     _ <- Either.cond(succeed, (), DecodingFailure.apply("Failed transaction", List()))
     version <- c.downField("version").as[Int]
     height <- c.downField("height").as[Int]
     typeId <- c.downField("type").as[Int]
     bodyBytes <- c.downField("bodyBytes").as[ByteString]
     proofs <- version match {
       case 1 => c.downField("signature").as[ByteString].map(v => List.apply(v))
       case _ => c.downField("proofs").as[List[ByteString]]
     }
     id <- c.downField("id").as[ByteString]
     recipient <- c.downField("recipient").as[ByteString]
     attachment <- c.downField("attachment").as[Option[Attachment]]
     senderPublicKey <- c.downField("senderPublicKey").as[ByteString]
     amount <- c.downField("amount").as[Long]
     fee <- c.downField("fee").as[Long]
     timestamp <- c.downField("timestamp").as[Long]
     assetId <- c.downField("assetId").as[Option[ByteString]]
     feeAssetId <- c.downField("feeAssetId").as[Option[ByteString]]
   } yield TransferTransaction(id, recipient, amount, assetId, feeAssetId, attachment.getOrElse(ANothing), fee, timestamp, height, typeId.toByte, version.toByte, senderPublicKey, proofs, bodyBytes)
}
