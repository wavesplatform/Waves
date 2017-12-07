package com.wavesplatform.state2

import cats.Monoid
import cats.implicits._
import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.db._
import com.wavesplatform.utils._
import org.iq80.leveldb.{DB, WriteBatch}
import scorex.account.{Address, Alias}

import scala.util.Try

class StateStorage private(db: DB) extends SubStorage(db, "state") with PropertiesStorage with VersionedStorage {

  import StateStorage._

  override protected val Version = 2

  private val HeightPrefix = "state-height".getBytes(Charset)
  private val TransactionsPrefix = "txs".getBytes(Charset)
  private val AccountTransactionsLengthsPrefix = "acc-txs-len".getBytes(Charset)
  private val WavesBalancePrefix = "waves-bal".getBytes(Charset)
  private val AddressesIndexPrefix = "add-idx".getBytes(Charset)
  private val AssetBalancePrefix = "asset-bal".getBytes(Charset)
  private val AddressAssetsPrefix = "address-assets".getBytes(Charset)
  private val AssetsPrefix = "assets".getBytes(Charset)
  private val OrderFillsPrefix = "ord-fls".getBytes(Charset)
  private val AccountTransactionIdsPrefix = "acc-ids".getBytes(Charset)
  private val PaymentTransactionHashesPrefix = "pmt-hash".getBytes(Charset)
  private val BalanceSnapshotsPrefix = "bal-snap".getBytes(Charset)
  private val LastBalanceHeightPrefix = "bal-h".getBytes(Charset)
  private val AliasToAddressPrefix = "alias-address".getBytes(Charset)
  private val AddressToAliasPrefix = "address-alias".getBytes(Charset)
  private val LeaseStatePrefix = "lease-state".getBytes(Charset)
  private val LeaseIndexPrefix = "lease-idx".getBytes(Charset)
  private val MaxAddress = "max-address"
  private val LeasesCount = "leases-count"

  def getHeight: Int = get(makeKey(HeightPrefix, 0)).map(Ints.fromByteArray).getOrElse(0)

  def setHeight(b: Option[WriteBatch], height: Int): Unit = put(makeKey(HeightPrefix, 0), Ints.toByteArray(height), b)

  def getTransaction(id: ByteStr): Option[(Int, Array[Byte])] =
    get(makeKey(TransactionsPrefix, id.arr)).map(TransactionsValueCodec.decode).map(_.explicitGet().value)

  def putTransactions(b: Option[WriteBatch], diff: Diff): Unit = {
    diff.transactions.foreach { case (id, (h, tx, _)) =>
      put(makeKey(TransactionsPrefix, id.arr), TransactionsValueCodec.encode((h, tx.bytes())))
    }

    putOrderFills(b, diff.orderFills)
    putPortfolios(b, diff.portfolios)
    putIssuedAssets(b, diff.issuedAssets)
    putAccountTransactionsIds(b, diff.accountTransactionIds)

    diff.paymentTransactionIdsByHashes.foreach { case (hash, id) =>
      put(makeKey(PaymentTransactionHashesPrefix, hash.arr), id.arr, b)
    }

    putAliases(b, diff.aliases)
    putLeases(b, diff.leaseState)

  }

  def getWavesBalance(address: Address): Option[(Long, Long, Long)] =
    get(makeKey(WavesBalancePrefix, address.bytes.arr)).map(WavesBalanceValueCodec.decode).map(_.explicitGet().value)

  def getAssetBalance(address: Address, asset: ByteStr): Option[Long] =
    get(makeKey(AssetBalancePrefix, Bytes.concat(address.bytes.arr, asset.arr))).map(Longs.fromByteArray)

  def getAssetInfo(asset: ByteStr): Option[AssetInfo] =
    get(makeKey(AssetsPrefix, asset.arr)).map(AssetInfoCodec.decode).map(_.explicitGet().value)

  def getAccountTransactionIds(address: Address, index: Int): Option[ByteStr] =
    get(makeKey(AccountTransactionIdsPrefix, accountIntKey(address, index))).map(b => ByteStr(b))

  def getAccountTransactionsLengths(address: Address): Option[Int] =
    get(makeKey(AccountTransactionsLengthsPrefix, address.bytes.arr)).map(Ints.fromByteArray)

  def getBalanceSnapshots(address: Address, height: Int): Option[(Int, Long, Long)] =
    get(makeKey(BalanceSnapshotsPrefix, accountIntKey(address, height))).map(BalanceSnapshotValueCodec.decode).map(_.explicitGet().value)

  def putBalanceSnapshots(b: Option[WriteBatch], address: Address, height: Int, value: (Int, Long, Long)): Unit =
    put(makeKey(BalanceSnapshotsPrefix, accountIntKey(address, height)), BalanceSnapshotValueCodec.encode(value), b)

  def putWavesBalance(b: Option[WriteBatch], address: Address, value: (Long, Long, Long)): Unit = {
    updateAddressesIndex(Seq(address.bytes.arr), b)
    put(makeKey(WavesBalancePrefix, address.bytes.arr), WavesBalanceValueCodec.encode(value), b)
  }

  def getPaymentTransactionHashes(hash: ByteStr): Option[ByteStr] =
    get(makeKey(PaymentTransactionHashesPrefix, hash.arr)).map(b => ByteStr(b))

  def getAddressOfAlias(alias: Alias): Option[Address] =
    get(makeKey(AliasToAddressPrefix, alias.bytes.arr)).map(b => Address.fromBytes(b).explicitGet())

  def getAddressAliases(address: Address): Option[Seq[Alias]] =
    get(makeKey(AddressToAliasPrefix, address.bytes.arr)).map(AliasSeqCodec.decode).map(_.explicitGet().value)

  def getOrderFills(id: ByteStr): Option[OrderFillInfo] =
    get(makeKey(OrderFillsPrefix, id.arr)).map(OrderFillInfoValueCodec.decode).map(_.explicitGet().value)

  def getLeaseState(id: ByteStr): Option[Boolean] = get(makeKey(LeaseStatePrefix, id.arr)).map(b => b.head == 1.toByte)

  def getActiveLeases: Option[Seq[ByteStr]] = {
    val count = getIntProperty(LeasesCount).getOrElse(0)
    val result = (0 until count).foldLeft(Seq.empty[ByteStr]) { (r, i) =>
      val maybeLease = get(makeKey(LeaseIndexPrefix, i))
      val maybeLeaseState = maybeLease.flatMap(l => get(makeKey(LeaseStatePrefix, l)).map(b => b.head == 1.toByte))
      if (maybeLeaseState.contains(true)) r :+ ByteStr(maybeLease.get) else r
    }
    if (result.isEmpty) None else Some(result)
  }

  def getLastBalanceSnapshotHeight(address: Address): Option[Int] =
    get(makeKey(LastBalanceHeightPrefix, address.bytes.arr)).map(Ints.fromByteArray)

  def putLastBalanceSnapshotHeight(b: Option[WriteBatch], address: Address, height: Int): Unit =
    put(makeKey(LastBalanceHeightPrefix, address.bytes.arr), Ints.toByteArray(height), b)

  def allWavesBalances: Map[Address, (Long, Long, Long)] = {
    val maxAddressIndex = getIntProperty(MaxAddress).getOrElse(0)
    log.info(s"Accounts count: $maxAddressIndex")
    (0 until maxAddressIndex).flatMap({ i =>
      val maybeAddressBytes = get(makeKey(AddressesIndexPrefix, i))
      val maybeAddress = maybeAddressBytes.flatMap { addressBytes => Address.fromBytes(addressBytes).toOption }
      val maybeBalances = maybeAddressBytes.flatMap { addressBytes =>
        get(makeKey(WavesBalancePrefix, addressBytes)).map(WavesBalanceValueCodec.decode).map(_.explicitGet().value)
      }
      (maybeAddress, maybeBalances) match {
        case (Some(a), Some(b)) => Some(a -> b)
        case _ => None
      }
    })(scala.collection.breakOut)
  }

  def allAssetsBalances: Map[Address, Map[ByteStr, Long]] = {
    val maxAddressIndex = getIntProperty(MaxAddress).getOrElse(0)
    log.info(s"Accounts count: $maxAddressIndex")
    (0 until maxAddressIndex).flatMap { i =>
      val maybeAddressBytes = get(makeKey(AddressesIndexPrefix, i))
      val maybeAddress = maybeAddressBytes.flatMap { addressBytes => Address.fromBytes(addressBytes).toOption }
      val maybeBalances = maybeAddressBytes.flatMap { addressBytes =>
        val maybeAssets = get(makeKey(AddressAssetsPrefix, addressBytes)).map(Id32SeqCodec.decode).map(_.explicitGet().value)
        maybeAssets.map { assets =>
          assets.foldLeft(Map.empty[ByteStr, Long]) { (m, a) =>
            val key = makeKey(AssetBalancePrefix, Bytes.concat(addressBytes, a.arr))
            val balance = get(key).map(Longs.fromByteArray).getOrElse(0L)
            m.updated(a, balance)
          }
        }
      }
      (maybeAddress, maybeBalances) match {
        case (Some(a), Some(b)) => Some(a -> b)
        case _ => None
      }
    }.toMap
  }

  def getAssetBalanceMap(address: Address): Option[Map[ByteStr, Long]] = {
    val assets = get(makeKey(AddressAssetsPrefix, address.bytes.arr)).map(Id32SeqCodec.decode).map(_.explicitGet().value)
    if (assets.isDefined) {
      val result = assets.get.foldLeft(Map.empty[ByteStr, Long]) { (m, a) =>
        val key = makeKey(AssetBalancePrefix, Bytes.concat(address.bytes.arr, a.arr))
        val balance = get(key).map(Longs.fromByteArray).getOrElse(0L)
        m.updated(a, balance)
      }
      Some(result)
    } else None
  }

  private def putOrderFills(b: Option[WriteBatch], fills: Map[ByteStr, OrderFillInfo]): Unit =
    fills.foreach { case (id, info) =>
      val key = makeKey(OrderFillsPrefix, id.arr)
      val existing = get(key).map(OrderFillInfoValueCodec.decode).map(_.explicitGet().value).getOrElse(Monoid[OrderFillInfo].empty)
      val updated = existing.combine(info)
      put(key, OrderFillInfoValueCodec.encode(updated), b)
    }

  private def updateAddressesIndex(addresses: Iterable[Array[Byte]], b: Option[WriteBatch]): Unit = {
    val n = getIntProperty(MaxAddress).getOrElse(0)
    val newAddresses = addresses.foldLeft(n) { (c, a) =>
      val key = makeKey(WavesBalancePrefix, a)
      if (get(key).isEmpty) {
        put(makeKey(AddressesIndexPrefix, c), a)
        c + 1
      } else c
    }
    putIntProperty(MaxAddress, newAddresses, b)
  }

  private def putPortfolios(b: Option[WriteBatch], portfolios: Map[Address, Portfolio]): Unit = {
    updateAddressesIndex(portfolios.keys.map(_.bytes.arr), b)
    portfolios.foreach { case (a, d) =>
      val addressBytes = a.bytes.arr

      val wavesKey = makeKey(WavesBalancePrefix, addressBytes)
      val existingWavesBalance = get(wavesKey).map(WavesBalanceValueCodec.decode).map(_.explicitGet().value).getOrElse((0L, 0L, 0L))
      val wavesBalanceValue = (
        existingWavesBalance._1 + d.balance,
        existingWavesBalance._2 + d.leaseInfo.leaseIn,
        existingWavesBalance._3 + d.leaseInfo.leaseOut
      )
      put(wavesKey, WavesBalanceValueCodec.encode(wavesBalanceValue), b)

      val addressKey = makeKey(AddressAssetsPrefix, addressBytes)
      val existingAssets = get(addressKey).map(Id32SeqCodec.decode).map(_.explicitGet().value).getOrElse(Seq.empty[ByteStr])
      val updatedAssets = existingAssets ++ d.assets.keySet
      put(addressKey, Id32SeqCodec.encode(updatedAssets.distinct))

      d.assets.foreach { case (as, df) =>
        val addressAssetKey = makeKey(AssetBalancePrefix, Bytes.concat(addressBytes, as.arr))
        val existingValue = get(addressAssetKey).map(Longs.fromByteArray).getOrElse(0L)
        val updatedValue = existingValue + df
        put(addressAssetKey, Longs.toByteArray(updatedValue), b)
      }
    }
  }

  private def putIssuedAssets(b: Option[WriteBatch], issues: Map[ByteStr, AssetInfo]): Unit =
    issues.foreach { case (id, info) =>
      val key = makeKey(AssetsPrefix, id.arr)
      val existing = get(key).map(AssetInfoCodec.decode).map(_.explicitGet().value).getOrElse(Monoid[AssetInfo].empty)
      put(key, AssetInfoCodec.encode(existing.combine(info)), b)
    }

  private def putAccountTransactionsIds(b: Option[WriteBatch], accountIds: Map[Address, List[ByteStr]]): Unit =
    accountIds.foreach { case (a, ids) =>
      val key = makeKey(AccountTransactionsLengthsPrefix, a.bytes.arr)
      val start = get(key).map(Ints.fromByteArray).getOrElse(0)
      val end = ids.reverse.foldLeft(start) { case (i, id) =>
        put(makeKey(AccountTransactionIdsPrefix, accountIntKey(a, i)), id.arr, b)
        i + 1
      }
      put(key, Ints.toByteArray(end), b)
    }

  private def putAliases(b: Option[WriteBatch], aliases: Map[Alias, Address]): Unit = {
    val map = aliases.foldLeft(Map.empty[Address, Seq[Alias]]) { (m, e) =>
      put(makeKey(AliasToAddressPrefix, e._1.bytes.arr), e._2.bytes.arr, b)
      val existingAliases = m.getOrElse(e._2, Seq.empty[Alias])
      val aliases = existingAliases :+ e._1
      m.updated(e._2, aliases.distinct)
    }
    map.foreach { e =>
      val key = makeKey(AddressToAliasPrefix, e._1.bytes.arr)
      val existing = get(key).map(AliasSeqCodec.decode).map(_.explicitGet().value).getOrElse(Seq.empty[Alias])
      val updated = existing ++ e._2
      put(key, AliasSeqCodec.encode(updated.distinct))
    }
  }

  private def putLeases(b: Option[WriteBatch], leases: Map[ByteStr, Boolean]): Unit = {
    val n = getIntProperty(LeasesCount).getOrElse(0)
    val count = leases.foldLeft(n) { (c, e) =>
      val key = makeKey(LeaseStatePrefix, e._1.arr)
      if (get(key).isDefined) {
        put(key, if (e._2) Codec.TrueBytes else Codec.FalseBytes, b)
        c
      } else {
        put(key, if (e._2) Codec.TrueBytes else Codec.FalseBytes, b)
        put(makeKey(LeaseIndexPrefix, c), e._1.arr, b)
        c + 1
      }
    }
    putIntProperty(LeasesCount, count, b)
  }
}

object StateStorage {

  def apply(db: DB, dropExisting: Boolean): Try[StateStorage] = createWithVerification[StateStorage](new StateStorage(db))

  def accountIntKey(acc: Address, index: Int): Array[Byte] = Bytes.concat(acc.bytes.arr, Ints.toByteArray(index))

}
