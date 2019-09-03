package com.wavesplatform.lang.v1.repl.model.tx

import com.wavesplatform.lang.v1.repl.model.{Account, ByteString}
import io.circe.{Decoder, DecodingFailure, HCursor}
import io.circe.generic.auto._

trait TransferTransaction {
  def id: ByteString
  def recipient: String
  def amount: Long
  def assetId: Option[String]
  def feeAssetId: Option[String]
  def attachment: ByteString
  def fee: Long
  def timestamp: Long
  def height: Int
  def `type`: Byte
  def version: Byte
  def senderPublicKey: Account
  def proofs: List[ByteString]
}

object TransferTransaction {
 implicit val decoder: Decoder[TransferTransaction] = (c: HCursor) =>
   for {
     version <- c.downField("version").as[Int]
     result  <- version match {
       case 1 => c.as[TransferTransactionV1]
       case 2 => c.as[TransferTransactionV2]
       case n => Left(DecodingFailure(s"Illegal transfer transaction version: $n", Nil))
     }
   } yield result
}
