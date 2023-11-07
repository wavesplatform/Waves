package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.serialization.*
import com.wavesplatform.state.DataEntry.Type
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, DataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.{DataTransaction, PBSince, TxPositiveAmount, TxVersion}
import com.wavesplatform.utils.StringBytes
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object DataTxSerializer {
  def toJson(tx: DataTransaction): JsObject = {
    import tx._
    BaseTxJson.toJson(tx) ++ Json.obj("data" -> Json.toJson(data))
  }

  def bodyBytes(tx: DataTransaction): Array[Byte] = {
    import tx._
    version match {
      case TxVersion.V1 =>
        Bytes.concat(
          Array(tpe.id.toByte, version),
          sender.arr,
          Shorts.toByteArray(data.size.toShort),
          Bytes.concat(data.map(serializeEntry)*),
          Longs.toByteArray(timestamp),
          Longs.toByteArray(fee.value)
        )

      case _ =>
        PBTransactionSerializer.bodyBytes(tx)
    }
  }

  private def serializeEntry(e: DataEntry[?]): Array[Byte] = {
    val keyBytes = e.key.utf8Bytes
    val valueBytes = e match {
      case IntegerDataEntry(_, value) => Bytes.concat(Array(Type.Integer.id.toByte), Longs.toByteArray(value))
      case BooleanDataEntry(_, value) => Array(Type.Boolean.id, if (value) 1 else 0).map(_.toByte)
      case BinaryDataEntry(_, value)  => Bytes.concat(Array(Type.Binary.id.toByte), Deser.serializeArrayWithLength(value.arr))
      case StringDataEntry(_, value)  => Bytes.concat(Array(Type.String.id.toByte), Deser.serializeArrayWithLength(value.utf8Bytes))
      case other                      => throw new IllegalArgumentException(s"Unsupported data entry: $other")
    }
    Bytes.concat(Shorts.toByteArray(keyBytes.length.toShort), keyBytes, valueBytes)
  }

  def toBytes(tx: DataTransaction): Array[Byte] =
    if (PBSince.affects(tx)) PBTransactionSerializer.bytes(tx)
    else Bytes.concat(Array(0: Byte), this.bodyBytes(tx), tx.proofs.bytes())

  def parseBytes(bytes: Array[Byte]): Try[DataTransaction] = Try {
    def parseDataEntries(buf: ByteBuffer): Seq[DataEntry[_]] = {
      val entryCount = buf.getShort
      require(entryCount >= 0 && buf.remaining() > entryCount, s"Broken array size ($entryCount entries while ${buf.remaining()} bytes available)")
      Vector.fill(entryCount)(parseEntry(buf))
    }

    val buf = ByteBuffer.wrap(bytes)
    require(buf.getByte == 0 && buf.getByte == DataTransaction.typeId && buf.getByte == 1, "transaction type mismatch")

    val sender    = buf.getPublicKey
    val data      = parseDataEntries(buf)
    val timestamp = buf.getLong // Timestamp before fee
    val fee       = TxPositiveAmount.unsafeFrom(buf.getLong)
    DataTransaction(TxVersion.V1, sender, data, fee, timestamp, buf.getProofs, AddressScheme.current.chainId)
  }

  private def parseEntry(buf: ByteBuffer): DataEntry[_] = {
    val key = new String(Deser.parseArrayWithLength(buf), UTF_8)
    buf.get match {
      case t if t == Type.Integer.id => IntegerDataEntry(key, buf.getLong)
      case t if t == Type.Boolean.id => BooleanDataEntry(key, buf.get != 0)
      case t if t == Type.Binary.id  => BinaryDataEntry(key, ByteStr(Deser.parseArrayWithLength(buf)))
      case t if t == Type.String.id  => StringDataEntry(key, new String(Deser.parseArrayWithLength(buf), UTF_8))
      case other                     => throw new IllegalArgumentException(s"Unknown type $other")
    }
  }
}
