package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.serialization.*
import com.wavesplatform.transaction.assets.BurnTransaction
import com.wavesplatform.transaction.{Proofs, TxPositiveAmount, TxNonNegativeAmount, TxVersion}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object BurnTxSerializer {
  def toJson(tx: BurnTransaction): Coeval[JsObject] = Coeval.evalOnce {
    import tx.*
    BaseTxJson.toJson(tx) ++
      Json.obj("assetId" -> asset.id.toString, "amount" -> quantity.value) ++
      (if (version == TxVersion.V2) Json.obj("chainId" -> chainId) else JsObject.empty)
  }

  def bodyBytes(tx: BurnTransaction): Coeval[Array[Byte]] = Coeval.evalOnce {
    import tx.*
    lazy val baseBytes = Bytes.concat(
      sender.arr,
      asset.id.arr,
      Longs.toByteArray(quantity.value),
      Longs.toByteArray(fee.value),
      Longs.toByteArray(timestamp)
    )

    version match {
      case TxVersion.V1 => Bytes.concat(Array(tpe.id.toByte), baseBytes)
      case TxVersion.V2 => Bytes.concat(Array(tpe.id.toByte, version, chainId), baseBytes)
      case _            => PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: BurnTransaction): Coeval[Array[Byte]] = tx.version match {
    case TxVersion.V1 => tx.bodyBytes.map(bb => Bytes.concat(bb, tx.proofs.toSignature.arr))
    case TxVersion.V2 => tx.bodyBytes.map(bb => Bytes.concat(Array(0: Byte), bb, tx.proofs.bytes()))
    case _            => Coeval.evalOnce(PBTransactionSerializer.bytes(tx))
  }

  def parseBytes(bytes: Array[Byte]): Try[BurnTransaction] = Try {
    def parseCommonPart(version: TxVersion, buf: ByteBuffer): BurnTransaction = {
      val sender    = buf.getPublicKey
      val asset     = buf.getIssuedAsset
      val quantity  = TxNonNegativeAmount.unsafeFrom(buf.getLong)
      val fee       = TxPositiveAmount.unsafeFrom(buf.getLong)
      val timestamp = buf.getLong
      BurnTransaction(version, sender, asset, quantity, fee, timestamp, Nil, AddressScheme.current.chainId)
    }

    require(bytes.length > 2, "buffer underflow while parsing transaction")

    if (bytes(0) == 0) {
      require(bytes(1) == BurnTransaction.typeId, "transaction type mismatch")
      val buf = ByteBuffer.wrap(bytes, 4, bytes.length - 4)
      parseCommonPart(TxVersion.V2, buf).copy(proofs = buf.getProofs)
    } else {
      require(bytes(0) == BurnTransaction.typeId, "transaction type mismatch")
      val buf = ByteBuffer.wrap(bytes, 1, bytes.length - 1)
      parseCommonPart(TxVersion.V1, buf).copy(proofs = Proofs(buf.getSignature))
    }
  }
}
