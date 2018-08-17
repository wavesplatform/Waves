package com.wavesplatform.matcher

import java.nio.ByteBuffer

import com.google.common.primitives.Longs
import com.wavesplatform.account.Address
import com.wavesplatform.database.Key
import com.wavesplatform.matcher.model.OrderInfo
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order, OrderV1, OrderV2}

import scala.util.Failure

object MatcherKeys {
  import com.wavesplatform.database.KeyHelpers._

  private def assetIdToBytes(assetId: Option[AssetId]): Array[Byte] = assetId.fold(Array.emptyByteArray)(_.arr)

  val version: Key[Int] = intKey(0, default = 1)

  def order(orderId: Array[Byte]): Key[Option[Order]] = Key.opt(
    bytes(1, orderId), { x =>
      val r = x.head match {
        case 1 => OrderV1.parseBytes(x.tail)
        case 2 => OrderV2.parseBytes(x.tail)
        case v => Failure(new NotImplementedError(s"Unknown version $v"))
      }

      r.recover {
        case e => throw new IllegalArgumentException(s"Can't parse $orderId order: ${e.getMessage}", e)
      }.get
    }, { x =>
      x.version +: x.bytes()
    }
  )

  def orderInfoOpt(orderId: Array[Byte]): Key[Option[OrderInfo]] = Key.opt(
    bytes(2, orderId),
    orderInfoParser,
    x => throw new NotImplementedError(s"You can't write $x to the DB. Please use 'MatcherKeys.orderInfo' for this")
  )
  def orderInfo(orderId: Array[Byte]): Key[OrderInfo] = Key(
    bytes(2, orderId),
    Option(_).fold[OrderInfo](OrderInfo.empty)(orderInfoParser), { oi =>
      val allocateBytes = if (oi.unsafeTotalSpend.isEmpty) 33 else 41
      val buf = ByteBuffer
        .allocate(allocateBytes)
        .putLong(oi.amount)
        .putLong(oi.filled)
        .put(if (oi.canceled) 1.toByte else 0.toByte)
        .putLong(oi.minAmount.getOrElse(0L))
        .putLong(oi.remainingFee)

      oi.unsafeTotalSpend.foreach(buf.putLong)
      buf.array()
    }
  )
  private def orderInfoParser(input: Array[Byte]): OrderInfo = {
    val bb = ByteBuffer.wrap(input)
    input.length match {
      case 17 => OrderInfo(bb.getLong, bb.getLong, bb.get == 1, None, 0, None)
      case 33 => OrderInfo(bb.getLong, bb.getLong, bb.get == 1, Some(bb.getLong), bb.getLong, None)
      case 41 => OrderInfo(bb.getLong, bb.getLong, bb.get == 1, Some(bb.getLong), bb.getLong, Some(bb.getLong))
    }
  }

  def addressOrdersSeqNr(address: Address): Key[Int] = bytesSeqNr(3, address.bytes.arr)
  def addressOrders(address: Address, seqNr: Int): Key[Option[OrderAssets]] =
    Key.opt(hBytes(4, seqNr, address.bytes.arr), OrderAssets.read, OrderAssets.write)

  def openVolume(address: Address, assetId: Option[AssetId]): Key[Option[Long]] =
    Key.opt(bytes(5, address.bytes.arr ++ assetIdToBytes(assetId)), Longs.fromByteArray, Longs.toByteArray)
  def openVolumeSeqNr(address: Address): Key[Int] = bytesSeqNr(6, address.bytes.arr)
  def openVolumeAsset(address: Address, seqNr: Int): Key[Option[AssetId]] =
    Key(hBytes(7, seqNr, address.bytes.arr), Option(_).collect { case b if b.nonEmpty => ByteStr(b) }, assetIdToBytes)

  def orderTxIdsSeqNr(orderId: ByteStr): Key[Int]           = bytesSeqNr(8, orderId.arr)
  def orderTxId(orderId: ByteStr, seqNr: Int): Key[ByteStr] = Key(hBytes(9, seqNr, orderId.arr), ByteStr(_), _.arr)

  def exchangeTransaction(txId: ByteStr): Key[Option[ExchangeTransaction]] =
    Key.opt(bytes(10, txId.arr), ExchangeTransaction.parse(_).get, _.bytes())
}
