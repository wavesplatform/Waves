package com.wavesplatform.matcher

import java.nio.ByteBuffer

import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.account.Address
import com.wavesplatform.database.Key
import com.wavesplatform.matcher.model.OrderInfo
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange._

import scala.util.Failure

object MatcherKeys {
  import com.wavesplatform.database.KeyHelpers._

  private def assetIdToBytes(assetId: Option[AssetId]): Array[Byte] = assetId.fold(Array.emptyByteArray)(_.arr)

  val version: Key[Int] = intKey(0, default = 1)

  def order(orderId: ByteStr): Key[Option[Order]] = Key.opt(
    bytes(1, orderId.arr), { x =>
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

  val OrderInfoPrefix = 2.toShort

  def orderInfoOpt(orderId: ByteStr): Key[Option[OrderInfo]] = Key.opt(
    bytes(2, orderId.arr),
    decodeOrderInfo,
    unsupported("You can't write Option[OrderInfo] to the DB. Please use 'MatcherKeys.orderInfo' for this")
  )

  def orderInfo(orderId: ByteStr): Key[OrderInfo] = Key(
    bytes(OrderInfoPrefix, orderId.arr),
    Option(_).fold[OrderInfo](OrderInfo.empty)(decodeOrderInfo), { oi =>
      val allocateBytes = if (oi.unsafeTotalSpend.isEmpty) 33 else 41
      val buf = ByteBuffer
        .allocate(allocateBytes)
        .putLong(oi.amount)
        .putLong(oi.filled)
        .put(oi.canceledByUser.fold(0: Byte)(if (_) 1 else 2))
        .putLong(oi.minAmount.getOrElse(0L))
        .putLong(oi.remainingFee)

      oi.unsafeTotalSpend.foreach(buf.putLong)
      buf.array()
    }
  )

  def decodeOrderInfo(input: Array[Byte]): OrderInfo = {
    def canceledByUser(x: Byte): Option[Boolean] = x match {
      case 0 => None
      case 1 => Some(true)
      case 2 => Some(false)
    }

    val bb = ByteBuffer.wrap(input)
    input.length match {
      case 17 => OrderInfo(bb.getLong, bb.getLong, canceledByUser(bb.get), None, 0, None)
      case 33 => OrderInfo(bb.getLong, bb.getLong, canceledByUser(bb.get), Some(bb.getLong), bb.getLong, None)
      case 41 => OrderInfo(bb.getLong, bb.getLong, canceledByUser(bb.get), Some(bb.getLong), bb.getLong, Some(bb.getLong))
    }
  }

  def activeOrdersSeqNr(address: Address): Key[Option[Int]] = Key.opt(bytes(3, address.bytes.arr), Ints.fromByteArray, Ints.toByteArray)
  def activeOrders(address: Address, seqNr: Int): Key[ActiveOrdersIndex.Node] =
    Key(bytes(4, address.bytes.arr ++ Ints.toByteArray(seqNr)), ActiveOrdersIndex.Node.read, ActiveOrdersIndex.Node.write)

  def openVolume(address: Address, assetId: Option[AssetId]): Key[Option[Long]] =
    Key.opt(bytes(5, address.bytes.arr ++ assetIdToBytes(assetId)), Longs.fromByteArray, Longs.toByteArray)
  def openVolumeSeqNr(address: Address): Key[Int] = bytesSeqNr(6, address.bytes.arr)
  def openVolumeAsset(address: Address, seqNr: Int): Key[Option[AssetId]] =
    Key(hBytes(7, seqNr, address.bytes.arr), Option(_).collect { case b if b.nonEmpty => ByteStr(b) }, assetIdToBytes)

  def orderTxIdsSeqNr(orderId: ByteStr): Key[Int]           = bytesSeqNr(8, orderId.arr)
  def orderTxId(orderId: ByteStr, seqNr: Int): Key[ByteStr] = Key(hBytes(9, seqNr, orderId.arr), ByteStr(_), _.arr)

  def exchangeTransaction(txId: ByteStr): Key[Option[ExchangeTransaction]] =
    Key.opt(bytes(10, txId.arr), ExchangeTransaction.parse(_).get, _.bytes())

  def activeOrdersSize(address: Address): Key[Option[Int]] = Key.opt(bytes(11, address.bytes.arr), Ints.fromByteArray, Ints.toByteArray)
  def activeOrderSeqNr(address: Address, orderId: Order.Id): Key[Option[Int]] =
    Key.opt(bytes(12, address.bytes.arr ++ orderId.arr), Ints.fromByteArray, Ints.toByteArray)

  def finalizedCommonSeqNr(address: Address): Key[Option[Int]] = Key.opt(bytes(13, address.bytes.arr), Ints.fromByteArray, Ints.toByteArray)
  def finalizedCommon(address: Address, seqNr: Int): Key[Option[Order.Id]] =
    Key.opt(bytes(14, address.bytes.arr ++ Ints.toByteArray(seqNr)), ByteStr(_), _.arr)

  def finalizedPairSeqNr(address: Address, pair: AssetPair): Key[Option[Int]] =
    Key.opt(bytes(15, address.bytes.arr ++ pair.bytes), Ints.fromByteArray, Ints.toByteArray)

  def finalizedPair(address: Address, pair: AssetPair, seqNr: Int): Key[Option[Order.Id]] =
    Key.opt(bytes(16, address.bytes.arr ++ pair.bytes ++ Ints.toByteArray(seqNr)), ByteStr(_), _.arr)

}
