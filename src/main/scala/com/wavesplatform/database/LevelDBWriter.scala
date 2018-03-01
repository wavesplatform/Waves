package com.wavesplatform.database

import java.nio.ByteBuffer

import com.google.common.io.ByteStreams.{newDataInput, newDataOutput}
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.LeaseDetails
import org.iq80.leveldb.{DB, ReadOptions}
import scalikejdbc.using
import scorex.account.{Address, Alias}
import scorex.block.{Block, BlockHeader}
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{IssueTransaction, ReissueTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.smart.{Script, SetScriptTransaction}
import scorex.transaction.{CreateAliasTransaction, Transaction, TransactionParser}

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import com.wavesplatform.crypto

/** The following namespaces are used:
  *
  * address -> waves balance history[]
  * (H, address) -> waves balance
  * address -> lease balance history[]
  * (H, address) -> lease balance
  * address -> asset ids[]
  * (address, asset id) -> asset balance history[]
  * (H, address, asset ID) -> asset balance
  * tx id -> (height, tx bytes)
  * H -> changed addresses[]
  * H -> (address, asset id)[]
  * H -> block
  * H -> txs[]
  *
  */
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
      val ndo = newDataOutput(6 + addressIdBytes.length)
      ndo.writeShort(prefix)
      ndo.writeInt(height)
      ndo.write(addressIdBytes)
      ndo.toByteArray
    }

    private def writeIntSeq(values: Seq[Int]): Array[Byte] = {
      val ndo = newDataOutput()
      ndo.writeInt(values.length)
      values.foreach(ndo.writeInt)
      ndo.toByteArray
    }

    private def readIntSeq(data: Array[Byte]): Seq[Int] = Option(data).fold(Seq.empty[Int]) { d =>
      val ndi = newDataInput(d)
      (1 to ndi.readInt()).map(_ => ndi.readInt())
    }

    private def readTxIds(data: Array[Byte]): Seq[ByteStr] = Option(data).fold(Seq.empty[ByteStr]) { d =>
      val b = ByteBuffer.wrap(d)
      val ids = Seq.newBuilder[ByteStr]

      while (b.remaining() > 0) {
        val buffer = b.get() match {
          case crypto.DigestSize => new Array[Byte](crypto.DigestSize)
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
          case crypto.DigestSize => crypto.DigestSize.toByte
          case crypto.SignatureLength => crypto.SignatureLength.toByte
        }).put(id.arr)
      }
      b.array()
    }

    private def writeBigIntSeq(values: Seq[BigInt]) = {
      val ndo = newDataOutput()
      ndo.writeInt(values.size)
      for (v <- values) {
        val bytes = v.toByteArray
        ndo.writeByte(bytes.length)
        ndo.write(bytes)
      }
      ndo.toByteArray
    }

    private def readBigIntSeq(data: Array[Byte]) = Option(data).fold(Seq.empty[BigInt]) { d =>
      val ndi = newDataInput(d)
      val length = ndi.readInt()
      for (_ <- 0 until length) yield {
        val size = ndi.readByte()
        val bytes = new Array[Byte](size)
        ndi.readFully(bytes)
        BigInt(bytes)
      }
    }

    private def historyKey(prefix: Int, bytes: Array[Byte]) = Key(byteKey(prefix, bytes), readIntSeq, writeIntSeq)

    val version = Key[Int](Array(0, 0), Option(_).fold(1)(Ints.fromByteArray), Ints.toByteArray)
    val height = Key[Int](Array(0, 1), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)

    def score(height: Int) = Key[BigInt](h(2, height), Option(_).fold(BigInt(0))(BigInt(_)), _.toByteArray)

    private def blockAtHeight(height: Int) = h(3, height)
    def blockAt(height: Int) = Key.opt[Block](blockAtHeight(height), Block.parseBytes(_).get, _.bytes())
    def blockBytes(height: Int) = Key.opt[Array[Byte]](blockAtHeight(height), identity, identity)
    def blockHeader(height: Int) =
      Key.opt[(BlockHeader, Int)](blockAtHeight(height), b => (BlockHeader.parseBytes(b).get._1, b.length), _ => Array.empty)

    def heightOf(blockId: ByteStr) = Key.opt[Int](hash(4, blockId), Ints.fromByteArray, Ints.toByteArray)

    def wavesBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey(5, addressId.toByteArray)

    def wavesBalance(height: Int, addressId: BigInt) =
      Key[Long](addressWithH(6, height, addressId), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

    def assetList(addressId: BigInt) = Key[Set[ByteStr]](addr(7, addressId), readTxIds(_).toSet, assets => writeTxIds(assets.toSeq))
    def assetBalanceHistory(addressId: BigInt, assetId: ByteStr) = historyKey(8, addressId.toByteArray ++ assetId.arr)
    def assetBalance(height: Int, addressId: BigInt, assetId: ByteStr) =
      Key[Long](byteKeyWithH(9, height, addressId.toByteArray ++ assetId.arr), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

    private def readAssetInfo(data: Array[Byte]) = {
      val b = ByteBuffer.wrap(data)
      AssetInfo(b.get() == 1, b.getLong)
    }

    private def writeAssetInfo(ai: AssetInfo) =
      ByteBuffer.allocate(1 + 8).put((if (ai.isReissuable) 1 else 0): Byte).putLong(ai.volume.longValue()).array()

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
    def leaseBalance(height: Int, addressId: BigInt): Key[LeaseBalance] = Key(byteKeyWithH(13, height, addressId.toByteArray), readLeaseBalance, writeLeaseBalance)

    def leaseStatusHistory(leaseId: ByteStr): Key[Seq[Int]] = historyKey(14, leaseId.arr)
    def leaseStatus(height: Int, leaseId: ByteStr): Key[Boolean] = Key(byteKeyWithH(15, height, leaseId.arr), _(0) == 1, active => Array[Byte](if (active) 1 else 0))

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
      (Ints.fromByteArray(data), TransactionParser.parseBytes(data.drop(4)).get)

    private def writeTransactionInfo(txInfo: (Int, Transaction)) = {
      val (h, tx) = txInfo
      val txBytes = tx.bytes()
      ByteBuffer.allocate(4 + txBytes.length).putInt(h).put(txBytes).array()
    }

    def transactionInfo(txId: ByteStr): Key[Option[(Int, Transaction)]] = Key.opt(hash(18, txId), readTransactionInfo, writeTransactionInfo)

    def addressTransactionHistory(addressId: BigInt): Key[Seq[Int]] = historyKey(19, addressId.toByteArray)

    private def readTransactionIds(data: Array[Byte]) = Option(data).fold(Seq.empty[(Int, ByteStr)]) { d =>
      val b = ByteBuffer.wrap(d)
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
      val size = ids.foldLeft(0) { case (prev, (_, id)) => prev + 2 + id.arr.length }
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

    def addressOfAlias(alias: Alias): Key[Option[Address]] = Key.opt(byteKey(23, alias.bytes.arr), Address.fromBytes(_).explicitGet(), _.bytes.arr)

    val lastAddressId: Key[Option[BigInt]] = Key.opt(Array[Byte](0, 24), BigInt(_), _.toByteArray)
    def addressId(address: Address): Key[Option[BigInt]] = Key.opt(byteKey(25, address.bytes.arr), BigInt(_), _.toByteArray)

    def addressScript(addressId: BigInt): Key[Option[Script]] =
      Key.opt(byteKey(26, addressId.toByteArray), Script.fromBytes(_).right.get, _.bytes().arr)

    private def readFeatureMap(data: Array[Byte]): Map[Short, Int] = Option(data).fold(Map.empty[Short, Int]) { _ =>
      val b = ByteBuffer.wrap(data)
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

    def approvedFeatures: Key[Map[Short, Int]] = Key(Array[Byte](0, 27), readFeatureMap, writeFeatureMap)
    def activatedFeatures: Key[Map[Short, Int]] = Key(Array[Byte](0, 28), readFeatureMap, writeFeatureMap)
  }

  private def loadAssetInfo(db: ReadOnlyDB, assetId: ByteStr) = {
    db.get(k.assetInfoHistory(assetId)).headOption.map(h => db.get(k.assetInfo(h, assetId)))
  }

  private def loadLeaseStatus(db: ReadOnlyDB, leaseId: ByteStr) =
    db.get(k.leaseStatusHistory(leaseId)).headOption.fold(false)(h => db.get(k.leaseStatus(h, leaseId)))

  private def resolveAlias(db: ReadOnlyDB, alias: Alias) = db.get(k.addressOfAlias(alias))

  /** {{{
    * ([10, 7, 4], 5, 11) => [10, 7]
    * ([10, 7], 5, 11) => [10, 7, 1]
    * }}}
    */
  private def slice(v: Seq[Int], from: Int, to: Int): Seq[Int] = {
    val (c1, c2) = v.dropWhile(_ > to).partition(_ > from)
    c1 :+ c2.headOption.getOrElse(1)
  }

  /** {{{([15, 12, 3], [12, 5]) => [(15, 12), (12, 12), (3, 12), (3, 5), (3, 0)]}}} */
  private def merge(wbh: Seq[Int], lbh: Seq[Int]): Seq[(Int, Int)] = {
    val wi = wbh.iterator.buffered
    val li = lbh.iterator.buffered
    val result = Seq.newBuilder[(Int, Int)]

    while (wi.hasNext || li.hasNext) {
      val nextW = wi.headOption.getOrElse(0)
      val nextL = li.headOption.getOrElse(0)

      result += {
        if (nextW > nextL) (wi.next(), nextL)
        else if (nextW < nextL) (nextW, li.next())
        else (wi.next(), li.next())
      }
    }

    result.result()
  }
}

class LevelDBWriter(writableDB: DB, fs: FunctionalitySettings) extends Caches {
  import LevelDBWriter._

  private def readOnly[A](f: ReadOnlyDB => A): A = using(writableDB.getSnapshot) { s =>
    f(new ReadOnlyDB(writableDB, new ReadOptions().snapshot(s)))
  }

  private def readWrite[A](f: RW => A): A = using(new RW(writableDB)) { rw => f(rw) }

  override protected def loadMaxAddressId(): BigInt = readOnly { db => db.get(k.lastAddressId).getOrElse(BigInt(0)) }

  override protected def loadAddressId(address: Address): Option[BigInt] = readOnly(db => db.get(k.addressId(address)))

  override protected def loadHeight(): Int = readOnly(_.get(k.height))

  override protected def loadScore(): BigInt = readOnly { db => db.get(k.score(db.get(k.height))) }

  override protected def loadLastBlock(): Option[Block] = readOnly { db => db.get(k.blockAt(db.get(k.height))) }

  override protected def loadScript(address: Address): Option[Script] =
    readOnly { db => db.get(k.addressId(address)).flatMap(id => db.get(k.addressScript(id))) }

  private def loadFromHistory[A](db: ReadOnlyDB, address: Address, key: BigInt => Key[Seq[Int]], v: (Int, BigInt) => Key[A]) =
    for {
      id <- addressIdCache.get(address)
      lastChange <- db.get(key(id)).headOption
    } yield db.get(v(lastChange, id))

  private def loadPortfolio(address: Address, db: ReadOnlyDB) = Portfolio(
      loadFromHistory(db, address, k.wavesBalanceHistory, k.wavesBalance).getOrElse(0L),
      loadFromHistory(db, address, k.leaseBalanceHistory, k.leaseBalance).getOrElse(LeaseBalance.empty),
      (for {
        addressId <- addressIdCache.get(address).toSeq
        assetId <- db.get(k.assetList(addressId))
        h <- db.get(k.assetBalanceHistory(addressId, assetId)).headOption
      } yield assetId -> db.get(k.assetBalance(h, addressId, assetId))).toMap
    )

  override protected def loadPortfolio(address: Address): Portfolio = readOnly(db => loadPortfolio(address, db))

  override protected def loadAssetInfo(assetId: ByteStr): Option[AssetInfo] =
    readOnly(LevelDBWriter.loadAssetInfo(_, assetId))

  override protected def loadAssetDescription(assetId: ByteStr): Option[AssetDescription] = readOnly { db =>
    db.get(k.transactionInfo(assetId)) match {
      case Some((_, i: IssueTransaction)) =>
        val reissuable = LevelDBWriter.loadAssetInfo(db, assetId).map(_.isReissuable)
        Some(AssetDescription(i.sender, i.name, i.decimals, reissuable.getOrElse(i.reissuable)))
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
                                  scripts: Map[BigInt, Option[Script]]): Unit = readWrite { rw =>
    val expiredKeys = new ArrayBuffer[Array[Byte]]

    rw.put(k.height, height)
    rw.put(k.blockAt(height), Some(block))
    rw.put(k.heightOf(block.uniqueId), Some(height))


    for ((address, id) <- newAddresses) {
      rw.put(k.addressId(address), Some(id))
    }

    val threshold = height - 2000

    for ((address, balance) <- wavesBalances) {
      rw.put(k.wavesBalance(height, address), balance)
      expiredKeys ++= updateHistory(rw, k.wavesBalanceHistory(address), threshold, h => k.wavesBalance(h, address))
    }

    for ((address, leaseBalance) <- leaseBalances) {
      rw.put(k.leaseBalance(height, address), leaseBalance)
      expiredKeys ++= updateHistory(rw, k.leaseBalanceHistory(address), threshold, k.leaseBalance(_, address))
    }

    for ((orderId, volumeAndFee) <- filledQuantity) {
      val kk = k.filledVolumeAndFee(height, orderId)
      rw.put(kk, volumeAndFee)
      expiredKeys ++= updateHistory(rw, k.filledVolumeAndFeeHistory(orderId), threshold, k.filledVolumeAndFee(_, orderId))
    }

    val changedAssetBalances = Set.newBuilder[(BigInt, ByteStr)]
    for ((address, assets) <- assetBalances) {
      rw.put(k.assetList(address), rw.get(k.assetList(address)) ++ assets.keySet)
      for ((assetId, balance) <- assets) {
        changedAssetBalances += address -> assetId
        rw.put(k.assetBalance(height, address, assetId), balance)
        expiredKeys ++= updateHistory(rw, k.assetBalanceHistory(address, assetId), threshold, k.assetBalance(_, address, assetId))
      }
    }

    for ((leaseId, state) <- leaseStates) {
      rw.put(k.leaseStatusHistory(leaseId), height +: rw.get(k.leaseStatusHistory(leaseId)))
      rw.put(k.leaseStatus(height, leaseId), state)
    }

    for ((addressId, script) <- scripts) {
      rw.put(k.addressScript(addressId), script)
    }

    val accountTransactions = (for {
      (id, (tx, addresses)) <- transactions.toSeq
      address <- addresses
    } yield (address, (tx.transactionType.id, id))).groupBy(_._1).mapValues(_.map(_._2))

    for ((addressId, txs) <- accountTransactions) {
      val ktxh = k.addressTransactionHistory(addressId)
      rw.put(ktxh, height +: rw.get(ktxh))
      rw.put(k.addressTransactionIds(height, addressId), txs)
    }

    for ((id, (tx, _)) <- transactions) {
      tx match {
        case ca: CreateAliasTransaction => rw.put(k.addressOfAlias(ca.alias), Some(ca.sender.toAddress))
        case _ =>
      }

      rw.put(k.transactionInfo(id), Some((height, tx)))
    }

    val activationWindowSize = fs.activationWindowSize(height)
    if (height % activationWindowSize == 0) {
      val minVotes = fs.blocksForFeatureActivation(height)
      val newlyApprovedFeatures = featureVotes(height).collect {
        case (featureId, voteCount) if voteCount >= minVotes => featureId -> height
      }

      if (newlyApprovedFeatures.nonEmpty) {
        approvedFeaturesCache = newlyApprovedFeatures ++ rw.get(k.approvedFeatures)
        rw.put(k.approvedFeatures, approvedFeaturesCache)
        activatedFeaturesCache = newlyApprovedFeatures.mapValues(_ + activationWindowSize) ++ rw.get(k.activatedFeatures)
        rw.put(k.activatedFeatures, activatedFeaturesCache)
      }
    }

    rw.put(k.transactionIdsAtHeight(height), transactions.keys.toSeq)
    expiredKeys.foreach(rw.delete)
  }

  override protected def doRollback(targetBlockId: ByteStr): Seq[Block] = readWrite { rw =>
    rw.get(k.heightOf(targetBlockId)).fold(Seq.empty[Block]) { targetHeight =>
      rw.put(k.height, targetHeight)

      for {
        h <- height until targetHeight by -1
        blockAtHeight <- rw.get(k.blockAt(h))
      } yield {
        for (address <- rw.get(k.changedAddresses(h))) {
          for (assetId <- rw.get(k.assetList(address))) {
            rw.delete(k.assetBalance(h, address, assetId))
            val historyKey = k.assetBalanceHistory(address, assetId)
            rw.put(historyKey, rw.get(historyKey).filterNot(_ == h))
          }

          rw.delete(k.wavesBalance(h, address))
          val wbh = k.wavesBalanceHistory(address)
          rw.put(wbh, rw.get(wbh).filterNot(_ == h))

          rw.delete(k.leaseBalance(h, address))
          val lbh = k.leaseBalanceHistory(address)
          rw.put(lbh, rw.get(lbh).filterNot(_ == h))
        }

        for (txId <- rw.get(k.transactionIdsAtHeight(h))) {
          rw.get(k.transactionInfo(txId)) match {
            case Some((_, i: IssueTransaction)) =>
              rw.delete(k.assetInfo(h, i.id()))
              val kaih = k.assetInfoHistory(i.id())
              rw.put(kaih, rw.get(kaih).filter(_ != h))
            case Some((_, r: ReissueTransaction)) =>
              rw.delete(k.assetInfo(h, r.assetId))
              val kaih = k.assetInfoHistory(r.assetId)
              rw.put(kaih, rw.get(kaih).filter(_ != h))
            case Some((_, c: LeaseCancelTransaction)) =>
            case Some((_, l: LeaseTransaction)) =>
            case Some((_, x: ExchangeTransaction)) =>
            case Some((_, a: CreateAliasTransaction)) =>
            case Some((_, s: SetScriptTransaction)) =>
            case _ =>
          }
        }

        blockAtHeight
      }
    }
  }

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] = readOnly(db => db.get(k.transactionInfo(id)))

  override def addressTransactions(
      address: Address,
      types: Set[TransactionParser.TransactionType.Value],
      from: Int,
      count: Int): Seq[(Int, Transaction)] = readOnly { db =>
    val filter = types.map(_.id)

    val txs = for {
      addressId <- db.get(k.addressId(address)).toSeq.view
      ints = db.get(k.addressTransactionHistory(addressId))
      h <- ints.view
      (txType, txId) <- db.get(k.addressTransactionIds(h, addressId)).view
      if filter.isEmpty || filter(txType)
      (_, tx) <- db.get(k.transactionInfo(txId)).view
    } yield h -> tx

    txs.slice(from, count)
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
      for {
        (wh, lh) <- merge(
          slice(db.get(k.wavesBalanceHistory(addressId)), from, to),
          slice(db.get(k.leaseBalanceHistory(addressId)), from, to))
        wb = db.get(k.wavesBalance(wh, addressId))
        lb = db.get(k.leaseBalance(lh, addressId))
      } yield BalanceSnapshot(wh.max(lh), wb, lb.in, lb.out)
    }
  }

  override def activeLeases: Set[LeaseTransaction] = readOnly { db =>
    (1 to db.get(k.height)).view
      .flatMap(h => db.get(k.transactionIdsAtHeight(h)))
      .flatMap(id => db.get(k.transactionInfo(id)))
      .collect {
        case (_, lt: LeaseTransaction) if loadLeaseStatus(db, lt.id()) => lt
      }
      .toSet
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
    currentHeight until currentHeight - howMany by -1 map(h => db.get(k.blockHeader(h)).get._1.signerData.signature)
  }

  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Seq[ByteStr] = readOnly { db =>
    db.get(k.heightOf(parentSignature)).fold(Seq.empty[ByteStr]) { parentHeight =>
      (parentHeight + 1) to (parentHeight + howMany) map { h => db.get(k.blockHeader(h)).get._1.signerData.signature }
    }
  }

  override def parent(childId: ByteStr, back: Int): Option[Block] = readOnly { db =>
    db.get(k.heightOf(childId)).flatMap(h => db.get(k.blockAt(h - back)))
  }

  override def featureVotes(height: Int): Map[Short, Int] = readOnly { db =>
    fs.activationWindow(height)
      .flatMap(h => db.get(k.blockHeader(h)).fold(Seq.empty[Short])(_._1.featureVotes.toSeq))
      .groupBy(identity)
      .mapValues(_.size)
  }
}
