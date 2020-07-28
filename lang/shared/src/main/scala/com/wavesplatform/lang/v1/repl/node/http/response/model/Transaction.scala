package com.wavesplatform.lang.v1.repl.node.http.response.model

import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.lang.v1.traits.domain.Recipient.{Address, Alias}
import io.circe.{Decoder, HCursor}

private[node] case class TransferTransaction(
    id: ByteString,
    recipient: Recipient,
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
    proofs: List[ByteString],
    succeed: Boolean
)

object Transaction {
  private val aliasPattern = "alias:\\w:(\\w+)".r

  implicit val decoder: Decoder[TransferTransaction] = (c: HCursor) =>
    for {
      applicationStatus <- c.downField("applicationStatus").as[Option[String]]
      succeed = applicationStatus.fold(true)(_ == "succeeded")
      version <- c.downField("version").as[Int]
      height  <- c.downField("height").as[Option[Int]]
      typeId  <- c.downField("type").as[Int]
      proofs <- version match {
        case 1 => c.downField("signature").as[ByteString].map(v => List.apply(v))
        case _ => c.downField("proofs").as[List[ByteString]]
      }
      id <- c.downField("id").as[ByteString]
      recipient <- c.downField("recipient").as[String].flatMap {
        case aliasPattern(alias) => Right(Alias(alias))
        case _                   => c.downField("recipient").as[ByteString].map(b => Address(b.byteStr))
      }
      attachment      <- c.downField("attachment").as[ByteString]
      senderPublicKey <- c.downField("senderPublicKey").as[ByteString]
      amount          <- c.downField("amount").as[Long]
      fee             <- c.downField("fee").as[Long]
      timestamp       <- c.downField("timestamp").as[Long]
      assetId         <- c.downField("assetId").as[Option[ByteString]]
      feeAssetId      <- c.downField("feeAssetId").as[Option[ByteString]]
    } yield TransferTransaction(
      id,
      recipient,
      amount,
      assetId,
      feeAssetId,
      attachment,
      fee,
      timestamp,
      height.getOrElse(-1),
      typeId.toByte,
      version.toByte,
      senderPublicKey,
      proofs,
      succeed
    )
}
