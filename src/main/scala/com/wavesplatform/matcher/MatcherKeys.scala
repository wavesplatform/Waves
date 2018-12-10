package com.wavesplatform.matcher

import java.nio.ByteBuffer

import com.google.common.primitives.{Ints, Longs, Shorts}
import com.wavesplatform.account.Address
import com.wavesplatform.database.Key
import com.wavesplatform.matcher.model.OrderInfo
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange._

object MatcherKeys {
  import com.wavesplatform.database.KeyHelpers._

  private def assetIdToBytes(assetId: Option[AssetId]): Array[Byte] = assetId.fold(Array.emptyByteArray)(_.arr)

  val version: Key[Int] = intKey("matcher-version", 0, default = 1)

  def order(orderId: ByteStr): Key[Option[Order]] =
    Key.opt(
      "matcher-order",
      bytes(1, orderId.arr),
      xs =>
        xs.head match {
          case 1     => OrderV1.parseBytes(xs.tail).get
          case 2     => OrderV2.parseBytes(xs.tail).get
          case other => throw new IllegalArgumentException(s"Unexpected order version: $other")
      },
      o => o.version +: o.bytes()
    )

  val OrderInfoPrefix: Short = 2

  def orderInfoOpt(orderId: ByteStr): Key[Option[OrderInfo]] = Key.opt(
    "matcher-order-info-opt",
    bytes(2, orderId.arr),
    decodeOrderInfo,
    unsupported("You can't write Option[OrderInfo] to the DB. Please use 'MatcherKeys.orderInfo' for this")
  )

  def orderInfo(orderId: ByteStr): Key[OrderInfo] = Key(
    "matcher-order-info",
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

  def activeOrdersOldestSeqNr(address: Address): Key[Option[Int]] =
    Key.opt("matcher-active-orders-oldest-seq-nr", bytes(3, address.bytes.arr), Ints.fromByteArray, Ints.toByteArray)
  def activeOrdersSeqNr(address: Address): Key[Option[Int]] =
    Key.opt("matcher-active-orders-seq-nr", bytes(4, address.bytes.arr), Ints.fromByteArray, Ints.toByteArray)

  val ActiveOrdersPrefix: Short            = 5
  val ActiveOrdersPrefixBytes: Array[Byte] = Shorts.toByteArray(ActiveOrdersPrefix)
  val ActiveOrdersKeyName: String          = "matcher-active-orders"
  def activeOrders(address: Address, seqNr: Int): Key[Option[ActiveOrdersIndex.Node]] =
    Key.opt(ActiveOrdersKeyName,
            bytes(ActiveOrdersPrefix, address.bytes.arr ++ Ints.toByteArray(seqNr)),
            ActiveOrdersIndex.Node.read,
            ActiveOrdersIndex.Node.write)

  def openVolume(address: Address, assetId: Option[AssetId]): Key[Option[Long]] =
    Key.opt("matcher-open-volume", bytes(6, address.bytes.arr ++ assetIdToBytes(assetId)), Longs.fromByteArray, Longs.toByteArray)
  def openVolumeSeqNr(address: Address): Key[Int] = bytesSeqNr("matcher-open-volume-seq-nr", 7, address.bytes.arr)
  def openVolumeAsset(address: Address, seqNr: Int): Key[Option[AssetId]] =
    Key("matcher-open-volume-asset", hBytes(8, seqNr, address.bytes.arr), Option(_).collect { case b if b.nonEmpty => ByteStr(b) }, assetIdToBytes)

  def orderTxIdsSeqNr(orderId: ByteStr): Key[Int]           = bytesSeqNr("matcher-order-tx-ids-seq-nr", 9, orderId.arr)
  def orderTxId(orderId: ByteStr, seqNr: Int): Key[ByteStr] = Key("matcher-order-tx-id", hBytes(10, seqNr, orderId.arr), ByteStr(_), _.arr)

  def exchangeTransaction(txId: ByteStr): Key[Option[ExchangeTransaction]] =
    Key.opt("matcher-exchange-transaction", bytes(11, txId.arr), ExchangeTransaction.parse(_).get, _.bytes())

  def activeOrdersSize(address: Address): Key[Option[Int]] =
    Key.opt("matcher-active-orders-size", bytes(12, address.bytes.arr), Ints.fromByteArray, Ints.toByteArray)
  def activeOrderSeqNr(address: Address, orderId: Order.Id): Key[Option[Int]] =
    Key.opt("matcher-active-order-seq-nr", bytes(13, address.bytes.arr ++ orderId.arr), Ints.fromByteArray, Ints.toByteArray)

  def finalizedCommonSeqNr(address: Address): Key[Option[Int]] =
    Key.opt("matcher-finalized-common-seq-nr", bytes(14, address.bytes.arr), Ints.fromByteArray, Ints.toByteArray)

  val FinalizedCommonPrefix: Short            = 15
  val FinalizedCommonPrefixBytes: Array[Byte] = Shorts.toByteArray(FinalizedCommonPrefix)
  val FinalizedCommonKeyName: String          = "matcher-finalized-common"
  def finalizedCommon(address: Address, seqNr: Int): Key[Option[Order.Id]] =
    Key.opt(FinalizedCommonKeyName, bytes(FinalizedCommonPrefix, address.bytes.arr ++ Ints.toByteArray(seqNr)), ByteStr(_), _.arr)

  def finalizedPairSeqNr(address: Address, pair: AssetPair): Key[Option[Int]] =
    Key.opt("matcher-finalized-pair-seq-nr", bytes(16, address.bytes.arr ++ pair.bytes), Ints.fromByteArray, Ints.toByteArray)

  val FinalizedPairPrefix: Short            = 17
  val FinalizedPairPrefixBytes: Array[Byte] = Shorts.toByteArray(FinalizedPairPrefix)
  val FinalizedPairKeyName: String          = "matcher-finalized-pair"
  def finalizedPair(address: Address, pair: AssetPair, seqNr: Int): Key[Option[Order.Id]] =
    Key.opt("matcher-finalized-pair", bytes(FinalizedPairPrefix, address.bytes.arr ++ pair.bytes ++ Ints.toByteArray(seqNr)), ByteStr(_), _.arr)

  def lastCommandTimestamp(address: Address): Key[Option[Long]] =
    Key.opt("matcher-last-command-timestamp", bytes(18, address.bytes.arr), Longs.fromByteArray, Longs.toByteArray)
}
