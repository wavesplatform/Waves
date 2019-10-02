package com.wavesplatform.lang.v1.repl.node.http.response.model

import io.circe.generic.auto._
import io.circe.{Decoder, DecodingFailure, HCursor}

private[node] trait TransferTransaction {
  def id: ByteString
  def recipient: ByteString
  def amount: Long
  def assetId: Option[ByteString]
  def feeAssetId: Option[ByteString]
  def attachment: ByteString
  def fee: Long
  def timestamp: Long
  def height: Int
  def `type`: Byte
  def version: Byte
  def senderPublicKey: ByteString
  def proofs: List[ByteString]
}

private[node] case class TransferTransactionV1(
  id: ByteString,
  recipient: ByteString,
  amount: Long,
  assetId: Option[ByteString],
  feeAssetId: Option[ByteString],
  attachment: ByteString,
  fee: Long,
  timestamp: Long,
  height: Int,
  `type`: Byte,
  version: Byte,
  senderPublicKey: ByteString,
  signature: ByteString
) extends TransferTransaction {
  override def proofs: List[ByteString] = List(signature)
}

private[node] case class TransferTransactionV2(
  id: ByteString,
  recipient: ByteString,
  amount: Long,
  assetId: Option[ByteString],
  feeAssetId: Option[ByteString],
  attachment: ByteString,
  fee: Long,
  timestamp: Long,
  height: Int,
  `type`: Byte,
  version: Byte,
  senderPublicKey: ByteString,
  proofs: List[ByteString]
) extends TransferTransaction

private[node] object TransferTransaction {
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
