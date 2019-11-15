package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.serialization._
import com.wavesplatform.state.DataEntry
import com.wavesplatform.transaction.{DataTransaction, TxVersion}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object DataTxSerializer {
  def toJson(tx: DataTransaction): JsObject = {
    import tx._
    BaseTxJson.toJson(tx) ++ Json.obj(
      "data" -> Json.toJson(data)
    )
  }

  def bodyBytes(tx: DataTransaction): Array[Byte] = {
    import tx._
    Bytes.concat(
      Array(builder.typeId, version),
      sender,
      Shorts.toByteArray(data.size.toShort),
      Bytes.concat(data.view.map(_.toBytes): _*),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  def toBytes(tx: DataTransaction): Array[Byte] = {
    Bytes.concat(Array(0: Byte), this.bodyBytes(tx), tx.proofs.bytes())
  }

  def parseBytes(bytes: Array[Byte]): Try[DataTransaction] = Try {
    def parseDataEntries(buf: ByteBuffer): Seq[DataEntry[_]] = {
      val entryCount = buf.getShort
      require(entryCount < 0 || entryCount > buf.remaining(), s"Broken array size ($entryCount entries while ${buf.remaining()} bytes available)")
      Vector.fill(entryCount)(DataEntry.parse(buf))
    }

    val buf = ByteBuffer.wrap(bytes)
    require(buf.getByte == 0 && buf.getByte == DataTransaction.typeId && buf.getByte == 1, "transaction type mismatch")

    val sender    = buf.getPublicKey
    val data      = parseDataEntries(buf)
    val timestamp = buf.getLong // Timestamp before fee
    val fee       = buf.getLong
    DataTransaction(TxVersion.V1, sender, data, fee, timestamp, buf.getProofs)
  }
}
