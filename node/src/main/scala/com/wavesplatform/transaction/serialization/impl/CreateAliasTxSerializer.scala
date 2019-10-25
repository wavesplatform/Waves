package com.wavesplatform.transaction.serialization.impl

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.description._
import com.wavesplatform.transaction.serialization.TxSerializer
import com.wavesplatform.transaction.{CreateAliasTransaction, TransactionBytesDescription, TransactionBytesDescriptionFor}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object CreateAliasTxSerializer extends TxSerializer[CreateAliasTransaction] {
  override def bodyBytes(tx: CreateAliasTransaction): Array[Byte] = {
    import tx._

    val base = Bytes.concat(
      sender,
      Deser.serializeArray(alias.bytes.arr),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )

    version match {
      case 1 => Bytes.concat(Array(builder.typeId), base)
      case 2 => Bytes.concat(Array(builder.typeId, version), base)
    }
  }

  override def toBytes(tx: CreateAliasTransaction): Array[Byte] = {
    import tx._

    version match {
      case 1 => Bytes.concat(bodyBytes(tx), tx.signature)
      case 2 => Bytes.concat(Array(0: Byte), bodyBytes(tx), proofs.bytes())
    }
  }

  override def parseBytes(bytes: Array[Byte]): Try[CreateAliasTransaction] = {
    require(bytes.length > 3, "Invalid tx bytes")
    bytes.take(3) match {
      case Array(0, CreateAliasTransaction.typeId, 2) => ???
    }

  }

  override def toJson(tx: CreateAliasTransaction): JsObject = {
    import tx._
    ProvenTxJson.toJson(tx) ++ Json.obj(
      "version"   -> version,
      "alias"     -> alias.name,
      "fee"       -> fee,
      "timestamp" -> timestamp
    )
  }

  object DescV1 extends TransactionBytesDescriptionFor(CreateAliasTransaction) with TransactionBytesDescription.HardcodedVersion1 {
    val byteTailDescription: ByteEntity[CreateAliasTransaction] = {
      (
        PublicKeyBytes(tailIndex(1), "Sender's public key"),
        AliasBytes(tailIndex(2), "Alias object"),
        LongBytes(tailIndex(3), "Fee"),
        LongBytes(tailIndex(4), "Timestamp"),
        SignatureBytes(tailIndex(5), "Signature")
      ) mapN { (sender, alias, fee, ts, signature) =>
        CreateAliasTransaction(1.toByte, ts, sender, alias, fee, Vector(signature))
      }
    }
  }

  object DescV2 extends TransactionBytesDescriptionFor(CreateAliasTransaction) with TransactionBytesDescription.MultipleVersions {
    val byteTailDescription: ByteEntity[CreateAliasTransaction] = {
      (
        PublicKeyBytes(tailIndex(1), "Sender's public key"),
        AliasBytes(tailIndex(2), "Alias object"),
        LongBytes(tailIndex(3), "Fee"),
        LongBytes(tailIndex(4), "Timestamp"),
        ProofsBytes(tailIndex(5))
      ) mapN { (sender, alias, fee, ts, proofs) =>
        CreateAliasTransaction(2.toByte, ts, sender, alias, fee, proofs)
      }
    }
  }
}
