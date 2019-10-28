package com.wavesplatform.transaction.serialization.impl

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.description._
import com.wavesplatform.transaction.serialization.TxSerializer
import com.wavesplatform.transaction.{CreateAliasTransaction, Proofs, Transaction, TransactionBytesDescription, TransactionBytesDescriptionFor}
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Try}

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
      case 1 => Bytes.concat(this.bodyBytes(tx), tx.signature)
      case 2 => Bytes.concat(Array(0: Byte), this.bodyBytes(tx), proofs.bytes())
    }
  }

  override def parseBytes(bytes: Array[Byte]): Try[CreateAliasTransaction] = {
    require(bytes.length > 3, "Invalid tx bytes")
    bytes.take(3) match {
      case Array(CreateAliasTransaction.typeId, _, _) => DescV1.byteDescription.deserializeFromByteArray(bytes)
      case Array(0, CreateAliasTransaction.typeId, 2) => DescV2.byteDescription.deserializeFromByteArray(bytes)
      case Array(b1, b2, b3)                          => Failure(new IllegalArgumentException(s"Invalid tx header bytes: $b1, $b2, $b3"))
    }
  }

  override def toJson(tx: CreateAliasTransaction): JsObject = {
    import tx._
    ProvenTxJson.toJson(tx) ++ Json.obj(
      "version"   -> version,
      "alias"     -> alias.name,
      "fee"       -> fee,
      "timestamp" -> timestamp
    ) ++ ProvenTxJsonLegacySignature.onlyV1(tx)
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
        CreateAliasTransaction(Transaction.V1, sender, alias, fee, ts, Proofs(signature))
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
        CreateAliasTransaction(Transaction.V2, sender, alias, fee, ts, proofs)
      }
    }
  }
}
