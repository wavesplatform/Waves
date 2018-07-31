package com.wavesplatform.matcher

import java.nio.ByteBuffer

import com.wavesplatform.database.Key
import com.wavesplatform.matcher.model.OrderInfo
import com.wavesplatform.state.ByteStr
import scorex.account.Address
import scorex.transaction.AssetId
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.google.common.primitives.Longs

object MatcherKeys {
  import com.wavesplatform.database.KeyHelpers._

  private def assetIdToBytes(assetId: Option[AssetId]) = assetId.fold(Array.emptyByteArray)(_.arr)

  val version = intKey(0, default = 1)

  def order(orderId: ByteStr): Key[Option[Order]] = Key.opt(bytes(1, orderId.arr), Order.parseBytes(_).get, _.bytes())
  def orderInfo(orderId: ByteStr): Key[OrderInfo] = Key(
    bytes(2, orderId.arr),
    Option(_).fold[OrderInfo](OrderInfo.empty) { b =>
      val bb = ByteBuffer.wrap(b)
      OrderInfo(bb.getLong, bb.getLong, bb.get == 1)
    },
    oi => ByteBuffer.allocate(17).putLong(oi.amount).putLong(oi.filled).put(if (oi.canceled) 1.toByte else 0.toByte).array()
  )

  def addressOrdersSeqNr(address: Address): Key[Int]                = bytesSeqNr(3, address.bytes.arr)
  def addressOrders(address: Address, seqNr: Int): Key[OrderAssets] = Key(hBytes(4, seqNr, address.bytes.arr), OrderAssets.read, OrderAssets.write)

  def openVolume(address: Address, assetId: Option[AssetId]): Key[Option[Long]] =
    Key.opt(bytes(5, address.bytes.arr ++ assetIdToBytes(assetId)), Longs.fromByteArray, Longs.toByteArray)
  def openVolumeSeqNr(address: Address): Key[Int] = bytesSeqNr(6, address.bytes.arr)
  def openVolumeAsset(address: Address, seqNr: Int): Key[Option[AssetId]] =
    Key(hBytes(7, seqNr, address.bytes.arr), Option(_).map(ByteStr(_)), assetIdToBytes)

  def orderTxIdsSeqNr(orderId: ByteStr): Key[Int]           = bytesSeqNr(8, orderId.arr)
  def orderTxId(orderId: ByteStr, seqNr: Int): Key[ByteStr] = Key(hBytes(9, seqNr, orderId.arr), ByteStr(_), _.arr)

  def exchangeTransaction(txId: ByteStr): Key[Option[ExchangeTransaction]] =
    Key.opt(bytes(10, txId.arr), ExchangeTransaction.parseBytes(_).get, _.bytes())
}
