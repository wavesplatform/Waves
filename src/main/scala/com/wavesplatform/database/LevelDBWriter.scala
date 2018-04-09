package com.wavesplatform.database

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import com.google.common.io.ByteStreams.{newDataInput, newDataOutput}
import com.google.common.primitives.{Ints, Longs, Shorts}
import com.wavesplatform.crypto
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.LeaseDetails
import org.iq80.leveldb.{DB, ReadOptions}
import scorex.account.{Address, Alias}
import scorex.block.{Block, BlockHeader}
import scorex.transaction.Transaction.Type
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.{Script, ScriptReader}
import scorex.utils.ScorexLogging

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

object LevelDBWriter {

  trait Key[V] {
    def keyBytes: Array[Byte]

    def parse(bytes: Array[Byte]): V

    def encode(v: V): Array[Byte]

    override lazy val toString: String = BigInt(keyBytes).toString(16)
  }

  object Key {
    def apply[V](key: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[V] = new Key[V] {
      override def keyBytes: Array[Byte] = key

      override def parse(bytes: Array[Byte]) = parser(bytes)

      override def encode(v: V) = encoder(v)
    }

    def opt[V](key: Array[Byte], parser: Array[Byte] => V, encoder: V => Array[Byte]): Key[Option[V]] =
      apply[Option[V]](key, Option(_).map(parser), _.fold[Array[Byte]](null)(encoder))
  }

  object k {
    val UTF8 = StandardCharsets.UTF_8

    private def h(prefix: Int, height: Int): Array[Byte] = {
      val ndo = newDataOutput(6)
      ndo.writeShort(prefix)
      ndo.writeInt(height)
      ndo.toByteArray
    }

    private def byteKeyWithH(prefix: Int, height: Int, bytes: Array[Byte]) = {
      val ndo = newDataOutput(6 + bytes.length)
      ndo.writeShort(prefix)
      ndo.writeInt(height)
      ndo.write(bytes)
      ndo.toByteArray
    }

    private def byteKey(prefix: Int, bytes: Array[Byte]) = {
      val ndo = newDataOutput(2 + bytes.length)
      ndo.writeShort(prefix)
      ndo.write(bytes)
      ndo.toByteArray
    }

    private def addr(prefix: Int, address: BigInt) = byteKey(prefix, address.toByteArray)

    private def hash(prefix: Int, hashBytes: ByteStr) = byteKey(prefix, hashBytes.arr)

    private def addressWithH(prefix: Int, height: Int, addressId: BigInt): Array[Byte] = {
      val addressIdBytes = addressId.toByteArray
      val ndo            = newDataOutput(6 + addressIdBytes.length)
      ndo.writeShort(prefix)
      ndo.writeInt(height)
      ndo.write(addressIdBytes)
      ndo.toByteArray
    }

    private def writeIntSeq(values: Seq[Int]): Array[Byte] = {
      val ndo = newDataOutput(4 * values.length)
      values.foreach(ndo.writeInt)
      ndo.toByteArray
    }

    private def readIntSeq(data: Array[Byte]): Seq[Int] = Option(data).fold(Seq.empty[Int]) { d =>
      val ndi = newDataInput(d)
      (1 to d.length / 4).map(_ => ndi.readInt())
    }

    private def readStrings(data: Array[Byte]): Seq[String] = Option(data).fold(Seq.empty[String]) { d =>
      var i = 0
      val s = Seq.newBuilder[String]

      while (i < data.length) {
        val len = Shorts.fromByteArray(data.drop(i))
        s += new String(data, i + 2, len, UTF8)
        i += (2 + len)
      }
      s.result()
    }

    private def writeStrings(strings: Seq[String]): Array[Byte] = {
      val b = ByteBuffer.allocate(strings.map(_.getBytes(UTF8).length + 2).sum)
      for (s <- strings) {
        val bytes = s.getBytes(UTF8)
        b.putShort(bytes.length.toShort).put(bytes)
      }
      b.array()
    }

    private def readTxIds(data: Array[Byte]): Seq[ByteStr] = Option(data).fold(Seq.empty[ByteStr]) { d =>
      val b   = ByteBuffer.wrap(d)
      val ids = Seq.newBuilder[ByteStr]

      while (b.remaining() > 0) {
        val buffer = b.get() match {
          case crypto.DigestSize      => new Array[Byte](crypto.DigestSize)
          case crypto.SignatureLength => new Array[Byte](crypto.SignatureLength)
        }
        b.get(buffer)
        ids += ByteStr(buffer)
      }

      ids.result()
    }

    private def writeTxIds(ids: Seq[ByteStr]): Array[Byte] = {
      val b = ByteBuffer.allocate(ids.foldLeft(0) { case (prev, id) => prev + 1 + id.arr.length })
      for (id <- ids) {
        b.put(id.arr.length match {
            case crypto.DigestSize      => crypto.DigestSize.toByte
            case crypto.SignatureLength => crypto.SignatureLength.toByte
          })
          .put(id.arr)
      }
      b.array()
    }

    private def writeBigIntSeq(values: Seq[BigInt]) = {
      val ndo = newDataOutput()
      ndo.writeInt(values.size)
      for (v <- values) {
        ndo.writeBigInt(v)
      }
      ndo.toByteArray
    }

    private def readBigIntSeq(data: Array[Byte]) = Option(data).fold(Seq.empty[BigInt]) { d =>
      val ndi    = newDataInput(d)
      val length = ndi.readInt()
      for (_ <- 0 until length) yield ndi.readBigInt()
    }

    private def historyKey(prefix: Int, bytes: Array[Byte]) = Key(byteKey(prefix, bytes), readIntSeq, writeIntSeq)

    val version = Key[Int](Array(0, 0), Option(_).fold(1)(Ints.fromByteArray), Ints.toByteArray)
    val height  = Key[Int](Array(0, 1), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)

    def score(height: Int) = Key[BigInt](h(2, height), Option(_).fold(BigInt(0))(BigInt(_)), _.toByteArray)

    private def blockAtHeight(height: Int) = h(3, height)

    def blockAt(height: Int) = Key.opt[Block](blockAtHeight(height), Block.parseBytes(_).get, _.bytes())

    def blockBytes(height: Int) = Key.opt[Array[Byte]](blockAtHeight(height), identity, identity)

    def blockHeader(height: Int) =
      Key.opt[(BlockHeader, Int)](
        blockAtHeight(height),
        b => (BlockHeader.parseBytes(b).get._1, b.length),
        _ =>
          throw new UnsupportedOperationException("Can't write block headers")) // this dummy encoder is never used: we only store blocks, not block headers

    def heightOf(blockId: ByteStr) = Key.opt[Int](hash(4, blockId), Ints.fromByteArray, Ints.toByteArray)

    def wavesBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey(5, addressId.toByteArray)

    def wavesBalance(height: Int, addressId: BigInt) =
      Key[Long](addressWithH(6, height, addressId), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

    def assetList(addressId: BigInt) = Key[Set[ByteStr]](addr(7, addressId), readTxIds(_).toSet, assets => writeTxIds(assets.toSeq))

    def assetBalanceHistory(addressId: BigInt, assetId: ByteStr) = historyKey(8, addressId.toByteArray ++ assetId.arr)

    def assetBalance(height: Int, addressId: BigInt, assetId: ByteStr) =
      Key[Long](byteKeyWithH(9, height, addressId.toByteArray ++ assetId.arr), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

    private def readAssetInfo(data: Array[Byte]) = {
      val ndi = newDataInput(data)
      AssetInfo(ndi.readBoolean(), ndi.readBigInt(), ndi.readScriptOption())
    }

    private def writeAssetInfo(ai: AssetInfo): Array[Byte] = {
      val ndo = newDataOutput()
      ndo.writeBoolean(ai.isReissuable)
      ndo.writeBigInt(ai.volume)
      ndo.writeScriptOption(ai.script)
      ndo.toByteArray
    }

    def assetInfoHistory(assetId: ByteStr): Key[Seq[Int]] = historyKey(10, assetId.arr)

    def assetInfo(height: Int, assetId: ByteStr): Key[AssetInfo] = Key(byteKeyWithH(11, height, assetId.arr), readAssetInfo, writeAssetInfo)

    private def writeLeaseBalance(lb: LeaseBalance): Array[Byte] = {
      val ndo = newDataOutput()
      ndo.writeLong(lb.in)
      ndo.writeLong(lb.out)
      ndo.toByteArray
    }

    private def readLeaseBalance(data: Array[Byte]) = Option(data).fold(LeaseBalance.empty) { d =>
      val ndi = newDataInput(d)
      LeaseBalance(ndi.readLong(), ndi.readLong())
    }

    def leaseBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey(12, addressId.toByteArray)

    def leaseBalance(height: Int, addressId: BigInt): Key[LeaseBalance] =
      Key(byteKeyWithH(13, height, addressId.toByteArray), readLeaseBalance, writeLeaseBalance)

    def leaseStatusHistory(leaseId: ByteStr): Key[Seq[Int]] = historyKey(14, leaseId.arr)

    def leaseStatus(height: Int, leaseId: ByteStr): Key[Boolean] =
      Key(byteKeyWithH(15, height, leaseId.arr), _(0) == 1, active => Array[Byte](if (active) 1 else 0))

    def filledVolumeAndFeeHistory(orderId: ByteStr): Key[Seq[Int]] = historyKey(16, orderId.arr)

    private def readVolumeAndFee(data: Array[Byte]) = Option(data).fold(VolumeAndFee.empty) { d =>
      val ndi = newDataInput(d)
      VolumeAndFee(ndi.readLong(), ndi.readLong())
    }

    private def writeVolumeAndFee(vf: VolumeAndFee) = {
      val ndo = newDataOutput()
      ndo.writeLong(vf.volume)
      ndo.writeLong(vf.fee)
      ndo.toByteArray
    }

    def filledVolumeAndFee(height: Int, orderId: ByteStr): Key[VolumeAndFee] =
      Key(byteKeyWithH(17, height, orderId.arr), readVolumeAndFee, writeVolumeAndFee)

    private def readTransactionInfo(data: Array[Byte]) =
      (Ints.fromByteArray(data), TransactionParsers.parseBytes(data.drop(4)).get)

    private def readTransactionHeight(data: Array[Byte]): Int = Ints.fromByteArray(data)

    private def writeTransactionInfo(txInfo: (Int, Transaction)) = {
      val (h, tx) = txInfo
      val txBytes = tx.bytes()
      ByteBuffer.allocate(4 + txBytes.length).putInt(h).put(txBytes).array()
    }

    def transactionInfo(txId: ByteStr): Key[Option[(Int, Transaction)]] = Key.opt(hash(18, txId), readTransactionInfo, writeTransactionInfo)

    def transactionHeight(txId: ByteStr): Key[Option[Int]] =
      Key.opt(hash(18, txId), readTransactionHeight, _ => throw new UnsupportedOperationException("Can't write transaction height only"))

    def addressTransactionHistory(addressId: BigInt): Key[Seq[Int]] = historyKey(19, addressId.toByteArray)

    private def readTransactionIds(data: Array[Byte]) = Option(data).fold(Seq.empty[(Int, ByteStr)]) { d =>
      val b   = ByteBuffer.wrap(d)
      val ids = Seq.newBuilder[(Int, ByteStr)]
      while (b.hasRemaining) {
        ids += b.get.toInt -> {
          val buf = new Array[Byte](b.get)
          b.get(buf)
          ByteStr(buf)
        }
      }
      ids.result()
    }

    private def writeTransactionIds(ids: Seq[(Int, ByteStr)]) = {
      val size   = ids.foldLeft(0) { case (prev, (_, id)) => prev + 2 + id.arr.length }
      val buffer = ByteBuffer.allocate(size)
      for ((typeId, id) <- ids) {
        buffer.put(typeId.toByte).put(id.arr.length.toByte).put(id.arr)
      }
      buffer.array()
    }

    def addressTransactionIds(height: Int, addressId: BigInt): Key[Seq[(Int, ByteStr)]] =
      Key(byteKeyWithH(20, height, addressId.toByteArray), readTransactionIds, writeTransactionIds)

    def changedAddresses(height: Int): Key[Seq[BigInt]] = Key(h(21, height), readBigIntSeq, writeBigIntSeq)

    def transactionIdsAtHeight(height: Int): Key[Seq[ByteStr]] = Key(h(22, height), readTxIds, writeTxIds)

    def addressIdOfAlias(alias: Alias): Key[Option[BigInt]] = Key.opt(byteKey(23, alias.bytes.arr), BigInt(_), _.toByteArray)

    val lastAddressId: Key[Option[BigInt]] = Key.opt(Array[Byte](0, 24), BigInt(_), _.toByteArray)

    def addressId(address: Address): Key[Option[BigInt]] = Key.opt(byteKey(25, address.bytes.arr), BigInt(_), _.toByteArray)

    def idToAddress(id: BigInt): Key[Address] = Key(byteKey(26, id.toByteArray), Address.fromBytes(_).explicitGet(), _.bytes.arr)

    def addressScriptHistory(addressId: BigInt): Key[Seq[Int]] = historyKey(27, addressId.toByteArray)

    def addressScript(height: Int, addressId: BigInt): Key[Option[Script]] =
      Key.opt(byteKeyWithH(28, height, addressId.toByteArray), ScriptReader.fromBytes(_).right.get, _.bytes().arr)

    private def readFeatureMap(data: Array[Byte]): Map[Short, Int] = Option(data).fold(Map.empty[Short, Int]) { _ =>
      val b        = ByteBuffer.wrap(data)
      val features = Map.newBuilder[Short, Int]
      while (b.hasRemaining) {
        features += b.getShort -> b.getInt
      }

      features.result()
    }

    private def writeFeatureMap(features: Map[Short, Int]): Array[Byte] = {
      val b = ByteBuffer.allocate(features.size * 6)
      for ((featureId, height) <- features)
        b.putShort(featureId).putInt(height)

      b.array()
    }

    def approvedFeatures: Key[Map[Short, Int]] = Key(Array[Byte](0, 29), readFeatureMap, writeFeatureMap)

    def activatedFeatures: Key[Map[Short, Int]] = Key(Array[Byte](0, 30), readFeatureMap, writeFeatureMap)

    def dataKeyList(addressId: BigInt) = Key[Set[String]](addr(31, addressId), readStrings(_).toSet, keys => writeStrings(keys.toSeq))

    def dataHistory(addressId: BigInt, key: String): Key[Seq[Int]] = historyKey(32, addressId.toByteArray ++ key.getBytes(UTF8))

    def data(height: Int, addressId: BigInt, key: String): Key[Option[DataEntry[_]]] =
      Key.opt(byteKeyWithH(33, height, addressId.toByteArray ++ key.getBytes(UTF8)), DataEntry.parseValue(key, _, 0)._1, _.valueBytes)
  }

  private def loadAssetInfo(db: ReadOnlyDB, assetId: ByteStr) = {
    db.get(k.assetInfoHistory(assetId)).headOption.map(h => db.get(k.assetInfo(h, assetId)))
  }

  private def loadLeaseStatus(db: ReadOnlyDB, leaseId: ByteStr) =
    db.get(k.leaseStatusHistory(leaseId)).headOption.fold(false)(h => db.get(k.leaseStatus(h, leaseId)))

  private def resolveAlias(db: ReadOnlyDB, alias: Alias) = {
    db.get(k.addressIdOfAlias(alias)).map(addressId => db.get(k.idToAddress(addressId)))
  }

  /** {{{
    * ([10, 7, 4], 5, 11) => [10, 7, 4]
    * ([10, 7], 5, 11) => [10, 7, 1]
    * }}}
    */
  private[database] def slice(v: Seq[Int], from: Int, to: Int): Seq[Int] = {
    val (c1, c2) = v.dropWhile(_ > to).partition(_ > from)
    c1 :+ c2.headOption.getOrElse(1)
  }

  /** {{{([15, 12, 3], [12, 5]) => [(15, 12), (12, 12), (3, 12), (3, 5)]}}}
    *
    * @param wbh WAVES balance history
    * @param lbh Lease balance history
    */
  private[database] def merge(wbh: Seq[Int], lbh: Seq[Int]): Seq[(Int, Int)] = {
    @tailrec
    def recMerge(wh: Int, wt: Seq[Int], lh: Int, lt: Seq[Int], buf: ArrayBuffer[(Int, Int)]): ArrayBuffer[(Int, Int)] = {
      buf += wh -> lh
      if (wt.isEmpty && lt.isEmpty) {
        buf
      } else if (wt.isEmpty) {
        recMerge(wh, wt, lt.head, lt.tail, buf)
      } else if (lt.isEmpty) {
        recMerge(wt.head, wt.tail, lh, lt, buf)
      } else {
        if (wh >= lh) {
          recMerge(wt.head, wt.tail, lh, lt, buf)
        } else {
          recMerge(wh, wt, lt.head, lt.tail, buf)
        }
      }
    }

    recMerge(wbh.head, wbh.tail, lbh.head, lbh.tail, ArrayBuffer.empty)
  }
}

class LevelDBWriter(writableDB: DB, fs: FunctionalitySettings) extends Caches with ScorexLogging {

  import LevelDBWriter._

  private def readOnly[A](f: ReadOnlyDB => A): A = {
    val s = writableDB.getSnapshot
    try f(new ReadOnlyDB(writableDB, new ReadOptions().snapshot(s)))
    finally s.close()
  }

  private def readWrite[A](f: RW => A): A = {
    val rw = new RW(writableDB)
    try f(rw)
    finally rw.close()
  }

  override protected def loadMaxAddressId(): BigInt = readOnly { db =>
    db.get(k.lastAddressId).getOrElse(BigInt(0))
  }

  override protected def loadAddressId(address: Address): Option[BigInt] = readOnly(db => db.get(k.addressId(address)))

  override protected def loadHeight(): Int = readOnly(_.get(k.height))

  override protected def loadScore(): BigInt = readOnly { db =>
    db.get(k.score(db.get(k.height)))
  }

  override protected def loadLastBlock(): Option[Block] = readOnly { db =>
    db.get(k.blockAt(db.get(k.height)))
  }

  override protected def loadScript(address: Address): Option[Script] = readOnly { db =>
    addressIdCache.get(address).fold[Option[Script]](None) { addressId =>
      loadFromHistory[Option[Script]](db, addressId, k.addressScriptHistory, k.addressScript).flatten
    }
  }

  override def accountData(address: Address): AccountDataInfo = readOnly { db =>
    val data = for {
      addressId <- addressIdCache.get(address).toSeq
      key       <- db.get(k.dataKeyList(addressId))
      value     <- accountData(address, key)
    } yield key -> value
    AccountDataInfo(data.toMap)
  }

  override def accountData(address: Address, key: String): Option[DataEntry[_]] = readOnly { db =>
    addressIdCache.get(address).fold[Option[DataEntry[_]]](None) { addressId =>
      loadFromHistory[Option[DataEntry[_]]](db, addressId, k.dataHistory(_, key), k.data(_, _, key)).flatten
    }
  }

  private def loadFromHistory[A](db: ReadOnlyDB, addressId: BigInt, key: BigInt => Key[Seq[Int]], v: (Int, BigInt) => Key[A]) =
    for {
      lastChange <- db.get(key(addressId)).headOption
    } yield db.get(v(lastChange, addressId))

  private def loadLposPortfolio(db: ReadOnlyDB, addressId: BigInt) = Portfolio(
    loadFromHistory(db, addressId, k.wavesBalanceHistory, k.wavesBalance).getOrElse(0L),
    loadFromHistory(db, addressId, k.leaseBalanceHistory, k.leaseBalance).getOrElse(LeaseBalance.empty),
    Map.empty
  )

  private def loadPortfolio(db: ReadOnlyDB, addressId: BigInt) = loadLposPortfolio(db, addressId).copy(
    assets = (for {
      assetId <- db.get(k.assetList(addressId))
      h       <- db.get(k.assetBalanceHistory(addressId, assetId)).headOption
    } yield assetId -> db.get(k.assetBalance(h, addressId, assetId))).toMap
  )

  override protected def loadPortfolio(address: Address): Portfolio = readOnly { db =>
    addressIdCache.get(address).fold(Portfolio.empty)(loadPortfolio(db, _))
  }

  override protected def loadAssetInfo(assetId: ByteStr): Option[AssetInfo] =
    readOnly(LevelDBWriter.loadAssetInfo(_, assetId))

  override protected def loadAssetDescription(assetId: ByteStr): Option[AssetDescription] = readOnly { db =>
    db.get(k.transactionInfo(assetId)) match {
      case Some((_, i: IssueTransaction)) =>
        val ai = LevelDBWriter.loadAssetInfo(db, assetId).getOrElse(AssetInfo(false, 0, None))
        Some(AssetDescription(i.sender, i.name, i.description, i.decimals, ai.isReissuable, ai.volume, ai.script))
      case Some((_, i: SmartIssueTransaction)) =>
        val ai = LevelDBWriter.loadAssetInfo(db, assetId).getOrElse(AssetInfo(false, 0, None))
        Some(AssetDescription(i.sender, i.name, i.description, i.decimals, ai.isReissuable, ai.volume, ai.script))
      case _ => None
    }
  }

  override protected def loadVolumeAndFee(orderId: ByteStr): VolumeAndFee = readOnly { db =>
    db.get(k.filledVolumeAndFeeHistory(orderId)).headOption.fold(VolumeAndFee.empty)(h => db.get(k.filledVolumeAndFee(h, orderId)))
  }

  override protected def loadApprovedFeatures(): Map[Short, Int] = readOnly(_.get(k.approvedFeatures))

  override protected def loadActivatedFeatures(): Map[Short, Int] = fs.preActivatedFeatures ++ readOnly(_.get(k.activatedFeatures))

  private def updateHistory(rw: RW, key: Key[Seq[Int]], threshold: Int, kf: Int => Key[_]): Seq[Array[Byte]] = {
    val (c1, c2) = rw.get(key).partition(_ > threshold)
    rw.put(key, (height +: c1) ++ c2.headOption)
    c2.drop(1).map(kf(_).keyBytes)
  }

  override protected def doAppend(block: Block,
                                  newAddresses: Map[Address, BigInt],
                                  wavesBalances: Map[BigInt, Long],
                                  assetBalances: Map[BigInt, Map[ByteStr, Long]],
                                  leaseBalances: Map[BigInt, LeaseBalance],
                                  leaseStates: Map[ByteStr, Boolean],
                                  transactions: Map[ByteStr, (Transaction, Set[BigInt])],
                                  reissuedAssets: Map[ByteStr, AssetInfo],
                                  filledQuantity: Map[ByteStr, VolumeAndFee],
                                  scripts: Map[BigInt, Option[Script]],
                                  data: Map[BigInt, AccountDataInfo],
                                  aliases: Map[Alias, BigInt]): Unit = readWrite { rw =>
    val expiredKeys = new ArrayBuffer[Array[Byte]]

    rw.put(k.height, height)
    rw.put(k.blockAt(height), Some(block))
    rw.put(k.heightOf(block.uniqueId), Some(height))
    rw.put(k.lastAddressId, Some(loadMaxAddressId() + newAddresses.size))
    rw.put(k.score(height), rw.get(k.score(height - 1)) + block.blockScore())

    for ((address, id) <- newAddresses) {
      rw.put(k.addressId(address), Some(id))
      rw.put(k.idToAddress(id), address)
    }

    val threshold        = height - 2000
    val changedAddresses = Set.newBuilder[BigInt]

    for ((addressId, balance) <- wavesBalances) {
      rw.put(k.wavesBalance(height, addressId), balance)
      changedAddresses += addressId
      expiredKeys ++= updateHistory(rw, k.wavesBalanceHistory(addressId), threshold, h => k.wavesBalance(h, addressId))
    }

    for ((addressId, leaseBalance) <- leaseBalances) {
      rw.put(k.leaseBalance(height, addressId), leaseBalance)
      changedAddresses += addressId
      expiredKeys ++= updateHistory(rw, k.leaseBalanceHistory(addressId), threshold, k.leaseBalance(_, addressId))
    }

    for ((addressId, assets) <- assetBalances) {
      rw.put(k.assetList(addressId), rw.get(k.assetList(addressId)) ++ assets.keySet)
      changedAddresses += addressId
      for ((assetId, balance) <- assets) {
        rw.put(k.assetBalance(height, addressId, assetId), balance)
        expiredKeys ++= updateHistory(rw, k.assetBalanceHistory(addressId, assetId), threshold, k.assetBalance(_, addressId, assetId))
      }
    }

    rw.put(k.changedAddresses(height), changedAddresses.result().toSeq)

    for ((orderId, volumeAndFee) <- filledQuantity) {
      val kk = k.filledVolumeAndFee(height, orderId)
      rw.put(kk, volumeAndFee)
      expiredKeys ++= updateHistory(rw, k.filledVolumeAndFeeHistory(orderId), threshold, k.filledVolumeAndFee(_, orderId))
    }

    for ((assetId, assetInfo) <- reissuedAssets) {
      rw.put(k.assetInfo(height, assetId), assetInfo)
      expiredKeys ++= updateHistory(rw, k.assetInfoHistory(assetId), threshold, k.assetInfo(_, assetId))
    }

    for ((leaseId, state) <- leaseStates) {
      rw.put(k.leaseStatus(height, leaseId), state)
      expiredKeys ++= updateHistory(rw, k.leaseStatusHistory(leaseId), threshold, k.leaseStatus(_, leaseId))
    }

    for ((addressId, script) <- scripts) {
      expiredKeys ++= updateHistory(rw, k.addressScriptHistory(addressId), threshold, k.addressScript(_, addressId))
      script.foreach(s => rw.put(k.addressScript(height, addressId), Some(s)))
    }

    for ((addressId, addressData) <- data) {
      rw.put(k.dataKeyList(addressId), rw.get(k.dataKeyList(addressId)) ++ addressData.data.keySet)
      for ((key, value) <- addressData.data) {
        rw.put(k.data(height, addressId, key), Some(value))
        expiredKeys ++= updateHistory(rw, k.dataHistory(addressId, key), threshold, k.data(_, addressId, key))
      }
    }

    val accountTransactions = (for {
      (id, (tx, addresses)) <- transactions.toSeq
      addressId             <- addresses
    } yield (addressId, (tx.builder.typeId.toInt, id))).groupBy(_._1).mapValues(_.map(_._2))

    for ((addressId, txs) <- accountTransactions) {
      rw.put(k.addressTransactionIds(height, addressId), txs)
    }

    for ((alias, addressId) <- aliases) {
      rw.put(k.addressIdOfAlias(alias), Some(addressId))
    }

    for ((id, (tx, _)) <- transactions) {
      rw.put(k.transactionInfo(id), Some((height, tx)))
    }

    val activationWindowSize = fs.activationWindowSize(height)
    if (height % activationWindowSize == 0) {
      val minVotes = fs.blocksForFeatureActivation(height)
      val newlyApprovedFeatures = featureVotes(height).collect {
        case (featureId, voteCount) if voteCount + (if (block.featureVotes(featureId)) 1 else 0) >= minVotes => featureId -> height
      }

      if (newlyApprovedFeatures.nonEmpty) {
        approvedFeaturesCache = newlyApprovedFeatures ++ rw.get(k.approvedFeatures)
        rw.put(k.approvedFeatures, approvedFeaturesCache)

        val featuresToSave = newlyApprovedFeatures.mapValues(_ + activationWindowSize) ++ rw.get(k.activatedFeatures)

        activatedFeaturesCache = featuresToSave ++ fs.preActivatedFeatures
        rw.put(k.activatedFeatures, featuresToSave)
      }
    }

    rw.put(k.transactionIdsAtHeight(height), transactions.keys.toSeq)
    expiredKeys.foreach(rw.delete)
  }

  override protected def doRollback(targetBlockId: ByteStr): Seq[Block] = {
    readOnly(_.get(k.heightOf(targetBlockId))).fold(Seq.empty[Block]) { targetHeight =>
      log.debug(s"Rolling back to block $targetBlockId at $targetHeight")

      val discardedBlocks = Seq.newBuilder[Block]

      for (currentHeight <- height until targetHeight by -1) readWrite { rw =>
        rw.put(k.height, currentHeight - 1)

        for (addressId <- rw.get(k.changedAddresses(currentHeight))) {
          val address = rw.get(k.idToAddress(addressId))

          for (assetId <- rw.get(k.assetList(addressId))) {
            rw.delete(k.assetBalance(currentHeight, addressId, assetId))
            rw.filterHistory(k.assetBalanceHistory(addressId, assetId), currentHeight)
          }

          rw.delete(k.wavesBalance(currentHeight, addressId))
          rw.filterHistory(k.wavesBalanceHistory(addressId), currentHeight)

          rw.delete(k.leaseBalance(currentHeight, addressId))
          rw.filterHistory(k.leaseBalanceHistory(addressId), currentHeight)

          log.trace(s"Discarding portfolio for $address")

          portfolioCache.invalidate(address)
        }

        val txIdsAtHeight = k.transactionIdsAtHeight(currentHeight)
        for (txId <- rw.get(txIdsAtHeight)) {
          forgetTransaction(txId)
          val ktxId         = k.transactionInfo(txId)
          val Some((_, tx)) = rw.get(ktxId)

          rw.delete(ktxId)
          tx match {
            case _: GenesisTransaction                                                                                         => // genesis transaction can not be rolled back
            case _: PaymentTransaction | _: TransferTransaction | _: VersionedTransferTransaction | _: MassTransferTransaction => // balances already restored

            case _: IssueTransaction        => rollbackAssetInfo(rw, tx.id(), currentHeight)
            case tx: ReissueTransaction     => rollbackAssetInfo(rw, tx.assetId, currentHeight)
            case tx: BurnTransaction        => rollbackAssetInfo(rw, tx.assetId, currentHeight)
            case _: LeaseTransaction        => rollbackLeaseStatus(rw, tx.id(), currentHeight)
            case tx: LeaseCancelTransaction => rollbackLeaseStatus(rw, tx.leaseId, currentHeight)

            case tx: SetScriptTransaction =>
              val address = tx.sender.toAddress
              scriptCache.invalidate(address)
              addressIdCache.get(address).foreach { addressId =>
                rw.delete(k.addressScript(currentHeight, addressId))
                rw.filterHistory(k.addressScriptHistory(addressId), currentHeight)
              }

            case tx: DataTransaction =>
              val address = tx.sender.toAddress
              addressIdCache.get(address).foreach { addressId =>
                tx.data.foreach { e =>
                  rw.delete(k.data(currentHeight, addressId, e.key))
                  rw.filterHistory(k.dataHistory(addressId, e.key), currentHeight)
                }
              }

            case tx: CreateAliasTransaction => rw.delete(k.addressIdOfAlias(tx.alias))
            case tx: ExchangeTransaction =>
              rollbackOrderFill(rw, ByteStr(tx.buyOrder.id()), currentHeight)
              rollbackOrderFill(rw, ByteStr(tx.sellOrder.id()), currentHeight)
          }
        }

        rw.delete(txIdsAtHeight)

        val discardedBlock = rw
          .get(k.blockAt(currentHeight))
          .getOrElse(throw new IllegalArgumentException(s"No block at height $currentHeight"))

        discardedBlocks += discardedBlock

        rw.delete(k.blockAt(currentHeight))
        rw.delete(k.heightOf(discardedBlock.uniqueId))
      }

      log.debug(s"Rollback to block $targetBlockId at $targetHeight completed")

      discardedBlocks.result()
    }
  }

  private def rollbackAssetInfo(rw: RW, assetId: ByteStr, currentHeight: Int): Unit = {
    rw.delete(k.assetInfo(currentHeight, assetId))
    rw.filterHistory(k.assetInfoHistory(assetId), currentHeight)
    assetInfoCache.invalidate(assetId)
    assetDescriptionCache.invalidate(assetId)
  }

  private def rollbackOrderFill(rw: RW, orderId: ByteStr, currentHeight: Int): Unit = {
    rw.delete(k.filledVolumeAndFee(currentHeight, orderId))
    rw.filterHistory(k.filledVolumeAndFeeHistory(orderId), currentHeight)
    volumeAndFeeCache.invalidate(orderId)
  }

  private def rollbackLeaseStatus(rw: RW, leaseId: ByteStr, currentHeight: Int): Unit = {
    rw.delete(k.leaseStatus(currentHeight, leaseId))
    rw.filterHistory(k.leaseStatusHistory(leaseId), currentHeight)
  }

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] = readOnly(db => db.get(k.transactionInfo(id)))

  override def transactionHeight(id: ByteStr): Option[Int] = readOnly(db => db.get(k.transactionHeight(id)))

  override def addressTransactions(address: Address, types: Set[Type], count: Int, from: Int): Seq[(Int, Transaction)] = readOnly { db =>
    db.get(k.addressId(address)).fold(Seq.empty[(Int, Transaction)]) { addressId =>
      val txs = for {
        h              <- (db.get(k.height) to 1 by -1).view
        (txType, txId) <- db.get(k.addressTransactionIds(h, addressId))
        if types.isEmpty || types.contains(txType.toByte)
        (_, tx) <- db.get(k.transactionInfo(txId))
      } yield (h, tx)

      txs.slice(from, count).force
    }
  }

  override def resolveAlias(a: Alias): Option[Address] = readOnly(db => LevelDBWriter.resolveAlias(db, a))

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = readOnly { db =>
    db.get(k.transactionInfo(leaseId)) match {
      case Some((h, lt: LeaseTransaction)) =>
        Some(LeaseDetails(lt.sender, lt.recipient, h, lt.amount, loadLeaseStatus(db, leaseId)))
      case _ => None
    }
  }

  override def balanceSnapshots(address: Address, from: Int, to: Int): Seq[BalanceSnapshot] = readOnly { db =>
    db.get(k.addressId(address)).fold(Seq(BalanceSnapshot(1, 0, 0, 0))) { addressId =>
      val wbh = slice(db.get(k.wavesBalanceHistory(addressId)), from, to)
      val lbh = slice(db.get(k.leaseBalanceHistory(addressId)), from, to)
      for {
        (wh, lh) <- merge(wbh, lbh)
        wb = db.get(k.wavesBalance(wh, addressId))
        lb = db.get(k.leaseBalance(lh, addressId))
      } yield BalanceSnapshot(wh.max(lh), wb, lb.in, lb.out)
    }
  }

  override def allActiveLeases: Set[LeaseTransaction] = readOnly { db =>
    val txs = for {
      h  <- 1 to db.get(k.height)
      id <- db.get(k.transactionIdsAtHeight(h))
      if loadLeaseStatus(db, id)
      (_, tx) <- db.get(k.transactionInfo(id))
    } yield tx

    txs.collect { case lt: LeaseTransaction => lt }.toSet
  }

  override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]) = readOnly { db =>
    val b = Map.newBuilder[Address, A]
    for (id <- BigInt(1) to db.get(k.lastAddressId).getOrElse(BigInt(0))) {
      val address = db.get(k.idToAddress(id))
      pf.runWith(b += address -> _)(address -> loadLposPortfolio(db, id))
    }
    b.result()
  }

  override def scoreOf(blockId: ByteStr): Option[BigInt] = readOnly(db => db.get(k.heightOf(blockId)).map(h => db.get(k.score(h))))

  override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = readOnly(_.get(k.blockHeader(height)))

  override def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] =
    readOnly(db => db.get(k.heightOf(blockId)).flatMap(h => db.get(k.blockHeader(h))))

  override def blockBytes(height: Int): Option[Array[Byte]] = readOnly(_.get(k.blockBytes(height)))

  override def blockBytes(blockId: ByteStr): Option[Array[Byte]] =
    readOnly(db => db.get(k.heightOf(blockId)).flatMap(h => db.get(k.blockBytes(h))))

  override def heightOf(blockId: ByteStr): Option[Int] = readOnly(_.get(k.heightOf(blockId)))

  override def lastBlockIds(howMany: Int): immutable.IndexedSeq[ByteStr] = readOnly { db =>
    // since this is called from outside of the main blockchain updater thread, instead of using cached height,
    // explicitly read height from storage to make this operation atomic.
    val currentHeight = db.get(k.height)
    (currentHeight until (currentHeight - howMany).max(0) by -1)
      .map(h => db.get(k.blockHeader(h)).get._1.signerData.signature)
  }

  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = readOnly { db =>
    db.get(k.heightOf(parentSignature)).map { parentHeight =>
      (parentHeight until (parentHeight + howMany))
        .flatMap { h =>
          db.get(k.blockHeader(h))
        }
        .map { b =>
          b._1.signerData.signature
        }
    }
  }

  override def parent(block: Block, back: Int): Option[Block] = readOnly { db =>
    db.get(k.heightOf(block.reference)).flatMap(h => db.get(k.blockAt(h - back + 1)))
  }

  override def featureVotes(height: Int): Map[Short, Int] = readOnly { db =>
    fs.activationWindow(height)
      .flatMap(h => db.get(k.blockHeader(h)).fold(Seq.empty[Short])(_._1.featureVotes.toSeq))
      .groupBy(identity)
      .mapValues(_.size)
  }

  private def distribution(height: Int, historyKey: BigInt => Key[Seq[Int]], balanceKey: (Int, BigInt) => Key[Long]): Map[Address, Long] = readOnly {
    db =>
      val result        = Map.newBuilder[Address, Long]
      var lastAddressId = db.get(k.lastAddressId).getOrElse(BigInt(0))
      while (lastAddressId > 0) {
        val abh          = db.get(historyKey(lastAddressId))
        val actualHeight = abh.partition(_ > height)._2.headOption.getOrElse(1)
        val balance      = db.get(balanceKey(actualHeight, lastAddressId))
        if (balance > 0) {
          result += db.get(k.idToAddress(lastAddressId)) -> balance
        }
        lastAddressId -= 1
      }
      result.result()
  }

  override def assetDistribution(height: Int, assetId: ByteStr) =
    distribution(height, k.assetBalanceHistory(_, assetId), k.assetBalance(_, _, assetId))

  override def wavesDistribution(height: Int) =
    distribution(height, k.wavesBalanceHistory, k.wavesBalance)
}
