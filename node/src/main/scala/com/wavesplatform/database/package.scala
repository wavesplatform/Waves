package com.wavesplatform

import com.google.common.base.Charsets.UTF_8
import com.google.common.collect.{Interners, Maps}
import com.google.common.io.ByteStreams.{newDataInput, newDataOutput}
import com.google.common.io.{ByteArrayDataInput, ByteArrayDataOutput}
import com.google.common.primitives.{Ints, Longs}
import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.block.validation.Validators
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.*
import com.wavesplatform.database.protobuf as pb
import com.wavesplatform.database.protobuf.DataEntry.Value
import com.wavesplatform.database.protobuf.TransactionData.Transaction as TD
import com.wavesplatform.lang.script.ScriptReader
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions}
import com.wavesplatform.protobuf.{ByteStringExt, PBSnapshots}
import com.wavesplatform.state.*
import com.wavesplatform.state.StateHash.SectionId
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{
  EthereumTransaction,
  GenesisTransaction,
  PBSince,
  PaymentTransaction,
  Transaction,
  TransactionParsers,
  TxPositiveAmount,
  TxValidationError
}
import com.wavesplatform.utils.*
import monix.eval.Task
import monix.reactive.Observable
import org.rocksdb.*
import sun.nio.ch.Util
import supertagged.TaggedType

import java.nio.ByteBuffer
import java.util
import java.util.Map as JMap
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.{View, mutable}
import scala.jdk.CollectionConverters.*
import scala.util.Using

//noinspection UnstableApiUsage
package object database {
  final type DBEntry = JMap.Entry[Array[Byte], Array[Byte]]

  implicit class ByteArrayDataOutputExt(val output: ByteArrayDataOutput) extends AnyVal {
    def writeByteStr(s: ByteStr): Unit = {
      output.write(s.arr)
    }
  }

  implicit class ByteArrayDataInputExt(val input: ByteArrayDataInput) extends AnyVal {
    def readBytes(len: Int): Array[Byte] = {
      val arr = new Array[Byte](len)
      input.readFully(arr)
      arr
    }

    def readByteStr(len: Int): ByteStr = {
      ByteStr(readBytes(len))
    }
  }

  def writeIntSeq(values: Seq[Int]): Array[Byte] = {
    values.foldLeft(ByteBuffer.allocate(4 * values.length))(_ putInt _).array()
  }

  def readIntSeq(data: Array[Byte]): Seq[Int] = Option(data).fold(Seq.empty[Int]) { d =>
    val in = ByteBuffer.wrap(data)
    Seq.fill(d.length / 4)(in.getInt)
  }

  def readAddressIds(data: Array[Byte]): Seq[AddressId] = Option(data).fold(Seq.empty[AddressId]) { d =>
    require(d.length % java.lang.Long.BYTES == 0, s"Invalid data length: ${d.length}")
    val buffer = ByteBuffer.wrap(data)
    Seq.fill(d.length / java.lang.Long.BYTES)(AddressId(buffer.getLong))
  }

  def writeAddressIds(values: Seq[AddressId]): Array[Byte] =
    values.foldLeft(ByteBuffer.allocate(values.length * java.lang.Long.BYTES)) { case (buf, aid) => buf.putLong(aid.toLong) }.array()

  def readAssetIds(data: Array[Byte]): Seq[ByteStr] = Option(data).fold(Seq.empty[ByteStr]) { d =>
    require(d.length % transaction.AssetIdLength == 0, s"Invalid data length: ${d.length}")
    val buffer = ByteBuffer.wrap(d)
    Seq.fill(d.length / transaction.AssetIdLength) {
      val idBytes = new Array[Byte](transaction.AssetIdLength)
      buffer.get(idBytes)
      ByteStr(idBytes)
    }
  }

  def writeAssetIds(values: Seq[ByteStr]): Array[Byte] =
    values.foldLeft(ByteBuffer.allocate(values.length * transaction.AssetIdLength)) { case (buf, ai) => buf.put(ai.arr) }.array()

  def readStrings(data: Array[Byte]): Seq[String] = Option(data).fold(Seq.empty[String]) { _ =>
    var i = 0
    val s = Seq.newBuilder[String]

    while (i < data.length) {
      val len = ((data(i) << 8) | (data(i + 1) & 0xff)).toShort // Optimization
      s += new String(data, i + 2, len, UTF_8)
      i += (2 + len)
    }
    s.result()
  }

  def writeStrings(strings: Seq[String]): Array[Byte] = {
    val utfBytes = strings.toVector.map(_.utf8Bytes)
    utfBytes
      .foldLeft(ByteBuffer.allocate(utfBytes.map(_.length + 2).sum)) { case (buf, bytes) =>
        buf.putShort(bytes.length.toShort).put(bytes)
      }
      .array()
  }

  def readLeaseBalanceNode(data: Array[Byte]): LeaseBalanceNode = if (data != null && data.length == 20)
    LeaseBalanceNode(Longs.fromByteArray(data.take(8)), Longs.fromByteArray(data.slice(8, 16)), Height(Ints.fromByteArray(data.takeRight(4))))
  else LeaseBalanceNode.Empty

  def writeLeaseBalanceNode(leaseBalanceNode: LeaseBalanceNode): Array[Byte] =
    Longs.toByteArray(leaseBalanceNode.in) ++ Longs.toByteArray(leaseBalanceNode.out) ++ Ints.toByteArray(leaseBalanceNode.prevHeight)

  def readLeaseBalance(data: Array[Byte]): CurrentLeaseBalance = if (data != null && data.length == 24)
    CurrentLeaseBalance(
      Longs.fromByteArray(data.take(8)),
      Longs.fromByteArray(data.slice(8, 16)),
      Height(Ints.fromByteArray(data.slice(16, 20))),
      Height(Ints.fromByteArray(data.takeRight(4)))
    )
  else CurrentLeaseBalance.Unavailable

  def writeLeaseBalance(lb: CurrentLeaseBalance): Array[Byte] =
    Longs.toByteArray(lb.in) ++ Longs.toByteArray(lb.out) ++ Ints.toByteArray(lb.height) ++ Ints.toByteArray(lb.prevHeight)

  def writeLeaseDetails(ld: LeaseDetails): Array[Byte] =
    pb.LeaseDetails(
      ByteString.copyFrom(ld.sender.arr),
      Some(PBRecipients.create(ld.recipientAddress)),
      ld.amount.value,
      ByteString.copyFrom(ld.sourceId.arr),
      ld.height,
      ld.status match {
        case LeaseDetails.Status.Active => pb.LeaseDetails.CancelReason.Empty
        case LeaseDetails.Status.Cancelled(height, cancelTxId) =>
          pb.LeaseDetails.CancelReason
            .Cancelled(pb.LeaseDetails.Cancelled(height, cancelTxId.fold(ByteString.EMPTY)(id => ByteString.copyFrom(id.arr))))
        case LeaseDetails.Status.Expired(height) => pb.LeaseDetails.CancelReason.Expired(pb.LeaseDetails.Expired(height))
      }
    ).toByteArray

  def readLeaseDetails(data: Array[Byte]): LeaseDetails = {
    val d = pb.LeaseDetails.parseFrom(data)
    LeaseDetails(
      LeaseStaticInfo(
        d.senderPublicKey.toPublicKey,
        PBRecipients.toAddress(d.recipient.get, AddressScheme.current.chainId).explicitGet(),
        TxPositiveAmount.unsafeFrom(d.amount),
        d.sourceId.toByteStr,
        d.height
      ),
      d.cancelReason match {
        case pb.LeaseDetails.CancelReason.Expired(pb.LeaseDetails.Expired(height, _)) => LeaseDetails.Status.Expired(height)
        case pb.LeaseDetails.CancelReason.Cancelled(pb.LeaseDetails.Cancelled(height, transactionId, _)) =>
          LeaseDetails.Status.Cancelled(height, Some(transactionId.toByteStr).filter(!_.isEmpty))
        case pb.LeaseDetails.CancelReason.Empty => LeaseDetails.Status.Active
      }
    )
  }

  def readVolumeAndFeeNode(data: Array[Byte]): VolumeAndFeeNode = if (data != null && data.length == 20)
    VolumeAndFeeNode(Longs.fromByteArray(data.take(8)), Longs.fromByteArray(data.slice(8, 16)), Height(Ints.fromByteArray(data.takeRight(4))))
  else VolumeAndFeeNode.Empty

  def writeVolumeAndFeeNode(volumeAndFeeNode: VolumeAndFeeNode): Array[Byte] =
    Longs.toByteArray(volumeAndFeeNode.volume) ++ Longs.toByteArray(volumeAndFeeNode.fee) ++ Ints.toByteArray(volumeAndFeeNode.prevHeight)

  def readVolumeAndFee(data: Array[Byte]): CurrentVolumeAndFee = if (data != null && data.length == 24)
    CurrentVolumeAndFee(
      Longs.fromByteArray(data.take(8)),
      Longs.fromByteArray(data.slice(8, 16)),
      Height(Ints.fromByteArray(data.slice(16, 20))),
      Height(Ints.fromByteArray(data.takeRight(4)))
    )
  else CurrentVolumeAndFee.Unavailable

  def writeVolumeAndFee(vf: CurrentVolumeAndFee): Array[Byte] =
    Longs.toByteArray(vf.volume) ++ Longs.toByteArray(vf.fee) ++ Ints.toByteArray(vf.height) ++ Ints.toByteArray(vf.prevHeight)

  def readFeatureMap(data: Array[Byte]): Map[Short, Int] = Option(data).fold(Map.empty[Short, Int]) { _ =>
    val b        = ByteBuffer.wrap(data)
    val features = Map.newBuilder[Short, Int]
    while (b.hasRemaining) {
      features += b.getShort -> b.getInt
    }

    features.result()
  }

  def writeFeatureMap(features: Map[Short, Int]): Array[Byte] = {
    val b = ByteBuffer.allocate(features.size * 6)
    for ((featureId, height) <- features)
      b.putShort(featureId).putInt(height)

    b.array()
  }

  def readSponsorship(data: Array[Byte]): SponsorshipValue = {
    val ndi = newDataInput(data)
    SponsorshipValue(ndi.readLong())
  }

  def writeSponsorship(ai: SponsorshipValue): Array[Byte] = {
    val ndo = newDataOutput()
    ndo.writeLong(ai.minFee)
    ndo.toByteArray
  }

  def readAssetDetails(data: Array[Byte]): (AssetInfo, AssetVolumeInfo) = {

    val pbad = pb.AssetDetails.parseFrom(data)

    (
      AssetInfo(pbad.name, pbad.description, Height(pbad.lastRenamedAt)),
      AssetVolumeInfo(pbad.reissuable, BigInt(pbad.totalVolume.toByteArray))
    )
  }

  def writeAssetDetails(ai: (AssetInfo, AssetVolumeInfo)): Array[Byte] = {
    val (info, volumeInfo) = ai

    pb.AssetDetails(
      info.name,
      info.description,
      info.lastUpdatedAt,
      volumeInfo.isReissuable,
      ByteString.copyFrom(volumeInfo.volume.toByteArray)
    ).toByteArray
  }

  def writeBlockMeta(data: pb.BlockMeta): Array[Byte] = data.toByteArray

  def readBlockMeta(bs: Array[Byte]): pb.BlockMeta = pb.BlockMeta.parseFrom(bs)

  def readTransactionHNSeqAndType(bs: Array[Byte]): (Height, Seq[(Byte, TxNum, Int)]) = {
    val ndi          = newDataInput(bs)
    val height       = Height(ndi.readInt())
    val numSeqLength = ndi.readInt()

    (
      height,
      List.fill(numSeqLength) {
        val tp   = ndi.readByte()
        val num  = TxNum(ndi.readShort())
        val size = ndi.readInt()
        (tp, num, size)
      }
    )
  }

  def writeTransactionHNSeqAndType(v: (Height, Seq[(Byte, TxNum, Int)])): Array[Byte] = {
    val (height, numSeq) = v
    val numSeqLength     = numSeq.length

    val outputLength = 4 + 4 + numSeqLength * (1 + 2 + 4)
    val ndo          = newDataOutput(outputLength)

    ndo.writeInt(height)
    ndo.writeInt(numSeqLength)
    numSeq.foreach { case (tp, num, size) =>
      ndo.writeByte(tp)
      ndo.writeShort(num)
      ndo.writeInt(size)
    }

    ndo.toByteArray
  }

  def readStateHash(bs: Array[Byte]): StateHash = {
    val ndi           = newDataInput(bs)
    val sectionsCount = ndi.readByte()
    val sections = (0 until sectionsCount).map { _ =>
      val sectionId = ndi.readByte()
      val value     = ndi.readByteStr(DigestLength)
      SectionId(sectionId) -> value
    }
    val totalHash = ndi.readByteStr(DigestLength)
    StateHash(totalHash, sections.toMap)
  }

  def writeStateHash(sh: StateHash): Array[Byte] = {
    val sorted = sh.sectionHashes.toSeq.sortBy(_._1)
    val ndo    = newDataOutput(crypto.DigestLength + 1 + sorted.length * (1 + crypto.DigestLength))
    ndo.writeByte(sorted.length)
    sorted.foreach { case (sectionId, value) =>
      ndo.writeByte(sectionId.id.toByte)
      ndo.writeByteStr(value.ensuring(_.arr.length == DigestLength))
    }
    ndo.writeByteStr(sh.totalHash.ensuring(_.arr.length == DigestLength))
    ndo.toByteArray
  }

  private def readDataEntry(key: String)(bs: Array[Byte]): DataEntry[?] =
    pb.DataEntry.parseFrom(bs).value match {
      case Value.Empty              => EmptyDataEntry(key)
      case Value.IntValue(value)    => IntegerDataEntry(key, value)
      case Value.BoolValue(value)   => BooleanDataEntry(key, value)
      case Value.BinaryValue(value) => BinaryDataEntry(key, value.toByteStr)
      case Value.StringValue(value) => StringDataEntry(key, value)
    }

  private def writeDataEntry(e: DataEntry[?]): Array[Byte] =
    pb.DataEntry(e match {
      case IntegerDataEntry(_, value) => pb.DataEntry.Value.IntValue(value)
      case BooleanDataEntry(_, value) => pb.DataEntry.Value.BoolValue(value)
      case BinaryDataEntry(_, value)  => pb.DataEntry.Value.BinaryValue(ByteString.copyFrom(value.arr))
      case StringDataEntry(_, value)  => pb.DataEntry.Value.StringValue(value)
      case _: EmptyDataEntry          => pb.DataEntry.Value.Empty
    }).toByteArray

  def readCurrentData(key: String)(bs: Array[Byte]): CurrentData = if (bs == null) CurrentData.empty(key)
  else
    CurrentData(
      readDataEntry(key)(bs.drop(8)),
      Height(Ints.fromByteArray(bs.take(4))),
      Height(Ints.fromByteArray(bs.slice(4, 8)))
    )

  def writeCurrentData(cdn: CurrentData): Array[Byte] =
    Ints.toByteArray(cdn.height) ++ Ints.toByteArray(cdn.prevHeight) ++ writeDataEntry(cdn.entry)

  def readDataNode(key: String)(bs: Array[Byte]): DataNode = if (bs == null) DataNode.empty(key)
  else
    DataNode(readDataEntry(key)(bs.drop(4)), Height(Ints.fromByteArray(bs.take(4))))

  def writeDataNode(dn: DataNode): Array[Byte] =
    Ints.toByteArray(dn.prevHeight) ++ writeDataEntry(dn.entry)

  def readCurrentBalance(bs: Array[Byte]): CurrentBalance = if (bs != null && bs.length == 16)
    CurrentBalance(Longs.fromByteArray(bs.take(8)), Height(Ints.fromByteArray(bs.slice(8, 12))), Height(Ints.fromByteArray(bs.takeRight(4))))
  else CurrentBalance.Unavailable

  def writeCurrentBalance(balance: CurrentBalance): Array[Byte] =
    Longs.toByteArray(balance.balance) ++ Ints.toByteArray(balance.height) ++ Ints.toByteArray(balance.prevHeight)

  def readBalanceNode(bs: Array[Byte]): BalanceNode = if (bs != null && bs.length == 12)
    BalanceNode(Longs.fromByteArray(bs.take(8)), Height(Ints.fromByteArray(bs.takeRight(4))))
  else BalanceNode.Empty

  def writeBalanceNode(balance: BalanceNode): Array[Byte] =
    Longs.toByteArray(balance.balance) ++ Ints.toByteArray(balance.prevHeight)

  implicit class DBExt(val db: RocksDB) extends AnyVal {

    def readOnly[A](f: ReadOnlyDB => A): A = {
      Using.resource(db.getSnapshot) { s =>
        Using.resource(new ReadOptions().setSnapshot(s).setVerifyChecksums(false)) { ro =>
          f(new ReadOnlyDB(db, ro))
        }
      }(db.releaseSnapshot(_))
    }

    /** @note
      *   Runs operations in batch, so keep in mind, that previous changes don't appear lately in f
      */
    def readWrite[A](f: RW => A): A = {
      val snapshot     = db.getSnapshot
      val readOptions  = new ReadOptions().setSnapshot(snapshot).setVerifyChecksums(false)
      val batch        = new WriteBatch()
      val rw           = new RW(db, readOptions, batch)
      val writeOptions = new WriteOptions()
      try {
        val r = f(rw)
        db.write(writeOptions, batch)
        r
      } finally {
        readOptions.close()
        writeOptions.close()
        batch.close()
        db.releaseSnapshot(snapshot)
      }
    }

    def multiGetOpt[A](readOptions: ReadOptions, keys: Seq[Key[Option[A]]], valBufSize: Int): Seq[Option[A]] =
      multiGetOpt(readOptions, keys, getKeyBuffersFromKeys(keys), getValueBuffers(keys.size, valBufSize))

    def multiGetOpt[A](readOptions: ReadOptions, keys: Seq[Key[Option[A]]], valBufSizes: Seq[Int]): Seq[Option[A]] =
      multiGetOpt(readOptions, keys, getKeyBuffersFromKeys(keys), getValueBuffers(valBufSizes))

    def multiGet[A](readOptions: ReadOptions, keys: ArrayBuffer[Key[A]], valBufSizes: ArrayBuffer[Int]): View[A] =
      multiGet(readOptions, keys, getKeyBuffersFromKeys(keys), getValueBuffers(valBufSizes))

    def multiGet[A](readOptions: ReadOptions, keys: ArrayBuffer[Key[A]], valBufSize: Int): View[A] =
      multiGet(readOptions, keys, getKeyBuffersFromKeys(keys), getValueBuffers(keys.size, valBufSize))

    def multiGet[A](readOptions: ReadOptions, keys: Seq[Key[A]], valBufSize: Int): Seq[Option[A]] = {
      val keyBufs = getKeyBuffersFromKeys(keys)
      val valBufs = getValueBuffers(keys.size, valBufSize)

      val cfhs = keys.map(_.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily)).asJava
      val result = keys.view
        .zip(db.multiGetByteBuffers(readOptions, cfhs, keyBufs, valBufs).asScala)
        .map { case (parser, value) =>
          if (value.status.getCode == Status.Code.Ok) {
            val arr = new Array[Byte](value.requiredSize)
            value.value.get(arr)
            Util.releaseTemporaryDirectBuffer(value.value)
            Some(parser.parse(arr))
          } else None
        }
        .toSeq

      keyBufs.forEach(Util.releaseTemporaryDirectBuffer(_))
      result
    }

    def multiGetInts(readOptions: ReadOptions, keys: Seq[Key[Int]]): Seq[Option[Int]] = {
      val keyBytes = keys.map(_.keyBytes)
      val keyBufs  = getKeyBuffers(keyBytes)
      val valBufs  = getValueBuffers(keyBytes.size, 4)

      val cfhs = keys.map(_.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily)).asJava
      val result = db
        .multiGetByteBuffers(readOptions, cfhs, keyBufs, valBufs)
        .asScala
        .map { value =>
          if (value.status.getCode == Status.Code.Ok) {
            val h = Some(value.value.getInt)
            Util.releaseTemporaryDirectBuffer(value.value)
            h
          } else None
        }
        .toSeq

      keyBufs.forEach(Util.releaseTemporaryDirectBuffer(_))
      result
    }

    def multiGetFlat[A](readOptions: ReadOptions, keys: ArrayBuffer[Key[Option[A]]], valBufSizes: ArrayBuffer[Int]): Seq[A] = {
      val keyBufs = getKeyBuffersFromKeys(keys)
      val valBufs = getValueBuffers(valBufSizes)

      val cfhs = keys.map(_.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily)).asJava
      val result = keys.view
        .zip(db.multiGetByteBuffers(readOptions, cfhs, keyBufs, valBufs).asScala)
        .flatMap { case (parser, value) =>
          if (value.status.getCode == Status.Code.Ok) {
            val arr = new Array[Byte](value.requiredSize)
            value.value.get(arr)
            Util.releaseTemporaryDirectBuffer(value.value)
            parser.parse(arr)
          } else None
        }
        .toSeq

      keyBufs.forEach(Util.releaseTemporaryDirectBuffer(_))
      result
    }

    def get[A](key: Key[A]): A = key.parse(db.get(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), key.keyBytes))
    def get[A](key: Key[A], readOptions: ReadOptions): A =
      key.parse(db.get(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), readOptions, key.keyBytes))
    def has(key: Key[?]): Boolean = db.get(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), key.keyBytes) != null

    def iterateOver(tag: KeyTags.KeyTag, cfh: Option[ColumnFamilyHandle] = None)(f: DBEntry => Unit): Unit =
      iterateOver(tag.prefixBytes, cfh)(f)

    def iterateOver(prefix: Array[Byte], cfh: Option[ColumnFamilyHandle])(f: DBEntry => Unit): Unit = {
      @tailrec
      def loop(iter: RocksIterator): Unit = {
        if (iter.isValid && iter.key().startsWith(prefix)) {
          f(Maps.immutableEntry(iter.key(), iter.value()))
          iter.next()
          loop(iter)
        } else ()
      }

      val iterator = db.newIterator(cfh.getOrElse(db.getDefaultColumnFamily), new ReadOptions().setTotalOrderSeek(true))
      try {
        iterator.seek(prefix)
        loop(iterator)
      } finally iterator.close()
    }

    def resourceObservable: Observable[DBResource] = Observable.resource(Task(DBResource(db)))(r => Task(r.close()))

    def withResource[A](f: DBResource => A): A = {
      val resource = DBResource(db)
      try f(resource)
      finally resource.close()
    }

    private def getKeyBuffersFromKeys(keys: collection.Seq[Key[?]]): util.List[ByteBuffer] =
      keys.map { k =>
        val arr = k.keyBytes
        val b   = Util.getTemporaryDirectBuffer(arr.length)
        b.put(k.keyBytes).flip()
        b
      }.asJava

    private def getKeyBuffers(keys: collection.Seq[Array[Byte]]): util.List[ByteBuffer] =
      keys.map { k =>
        val b = Util.getTemporaryDirectBuffer(k.length)
        b.put(k).flip()
        b
      }.asJava

    private def getValueBuffers(amount: Int, bufferSize: Int): util.List[ByteBuffer] =
      List
        .fill(amount) {
          val buf = Util.getTemporaryDirectBuffer(bufferSize)
          buf.limit(buf.capacity())
          buf
        }
        .asJava

    private def getValueBuffers(bufferSizes: collection.Seq[Int]): util.List[ByteBuffer] =
      bufferSizes.map { size =>
        val buf = Util.getTemporaryDirectBuffer(size)
        buf.limit(buf.capacity())
        buf
      }.asJava

    private def multiGetOpt[A](
        readOptions: ReadOptions,
        keys: Seq[Key[Option[A]]],
        keyBufs: util.List[ByteBuffer],
        valBufs: util.List[ByteBuffer]
    ): Seq[Option[A]] = {
      val cfhs = keys.map(_.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily)).asJava
      val result = keys.view
        .zip(db.multiGetByteBuffers(readOptions, cfhs, keyBufs, valBufs).asScala)
        .map { case (parser, value) =>
          if (value.status.getCode == Status.Code.Ok) {
            val arr = new Array[Byte](value.requiredSize)
            value.value.get(arr)
            Util.releaseTemporaryDirectBuffer(value.value)
            parser.parse(arr)
          } else None
        }
        .toSeq

      keyBufs.forEach(Util.releaseTemporaryDirectBuffer(_))
      result
    }

    private def multiGet[A](
        readOptions: ReadOptions,
        keys: ArrayBuffer[Key[A]],
        keyBufs: util.List[ByteBuffer],
        valBufs: util.List[ByteBuffer]
    ): View[A] = {
      val cfhs = keys.map(_.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily)).asJava
      val result = keys.view
        .zip(db.multiGetByteBuffers(readOptions, cfhs, keyBufs, valBufs).asScala)
        .flatMap { case (parser, value) =>
          if (value.status.getCode == Status.Code.Ok) {
            val arr = new Array[Byte](value.requiredSize)
            value.value.get(arr)
            Util.releaseTemporaryDirectBuffer(value.value)
            Some(parser.parse(arr))
          } else None
        }

      keyBufs.forEach(Util.releaseTemporaryDirectBuffer(_))
      result
    }
  }

  def createBlock(header: BlockHeader, signature: ByteStr, txs: Seq[Transaction]): Either[TxValidationError.GenericError, Block] =
    Validators.validateBlock(Block(header, signature, txs))

  def writeAssetScript(script: AssetScriptInfo): Array[Byte] =
    Longs.toByteArray(script.complexity) ++ script.script.bytes().arr

  def readAssetScript(b: Array[Byte]): AssetScriptInfo =
    AssetScriptInfo(ScriptReader.fromBytes(b.drop(8)).explicitGet(), Longs.fromByteArray(b))

  def writeAccountScriptInfo(scriptInfo: AccountScriptInfo): Array[Byte] =
    pb.AccountScriptInfo.toByteArray(
      pb.AccountScriptInfo(
        ByteString.copyFrom(scriptInfo.publicKey.arr),
        ByteString.copyFrom(scriptInfo.script.bytes().arr),
        scriptInfo.verifierComplexity,
        scriptInfo.complexitiesByEstimator.map { case (version, complexities) =>
          pb.AccountScriptInfo.ComplexityByVersion(version, complexities)
        }.toSeq
      )
    )

  private val scriptInterner = Interners.newWeakInterner[AccountScriptInfo]()

  def readAccountScriptInfo(b: Array[Byte]): AccountScriptInfo = {
    val asi = pb.AccountScriptInfo.parseFrom(b)
    scriptInterner.intern(
      AccountScriptInfo(
        PublicKey(asi.publicKey.toByteArray),
        ScriptReader.fromBytes(asi.scriptBytes.toByteArray).explicitGet(),
        asi.maxComplexity,
        asi.callableComplexity.map { c =>
          c.version -> c.callableComplexity
        }.toMap
      )
    )
  }

  def readTransaction(height: Height)(b: Array[Byte]): (TxMeta, Transaction) = {
    val data = pb.TransactionData.parseFrom(b)
    TxMeta(height, TxMeta.Status.fromProtobuf(data.status), data.spentComplexity) -> toVanillaTransaction(data.transaction)
  }

  def toVanillaTransaction(tx: pb.TransactionData.Transaction): Transaction = tx match {
    case tx: TD.LegacyBytes         => TransactionParsers.parseBytes(tx.value.toByteArray).get
    case tx: TD.WavesTransaction    => PBTransactions.vanilla(tx.value, unsafe = false).explicitGet()
    case tx: TD.EthereumTransaction => EthereumTransaction(tx.value.toByteArray).explicitGet()
    case _                          => throw new IllegalArgumentException("Illegal transaction data")
  }

  def writeTransaction(v: (TxMeta, Transaction)): Array[Byte] = {
    val (m, tx) = v
    val ptx = tx match {
      case lps: PBSince if !lps.isProtobufVersion => TD.LegacyBytes(ByteString.copyFrom(tx.bytes()))
      case _: GenesisTransaction                  => TD.LegacyBytes(ByteString.copyFrom(tx.bytes()))
      case _: PaymentTransaction                  => TD.LegacyBytes(ByteString.copyFrom(tx.bytes()))
      case et: EthereumTransaction                => TD.EthereumTransaction(ByteString.copyFrom(et.bytes()))
      case _                                      => TD.WavesTransaction(PBTransactions.protobuf(tx))
    }
    pb.TransactionData(ptx, m.status.protobuf, m.spentComplexity).toByteArray
  }

  def loadTransactions(height: Height, rdb: RDB): Seq[(TxMeta, Transaction)] = {
    val transactions = Seq.newBuilder[(TxMeta, Transaction)]
    rdb.db.iterateOver(KeyTags.NthTransactionInfoAtHeight.prefixBytes ++ Ints.toByteArray(height), Some(rdb.txHandle.handle)) { e =>
      transactions += readTransaction(height)(e.getValue)
    }
    transactions.result()
  }

  def loadTxStateSnapshots(height: Height, rdb: RDB): Seq[TransactionStateSnapshot] = {
    val txSnapshots = Seq.newBuilder[TransactionStateSnapshot]
    rdb.db.iterateOver(KeyTags.NthTransactionStateSnapshotAtHeight.prefixBytes ++ Ints.toByteArray(height), Some(rdb.txSnapshotHandle.handle)) { e =>
      txSnapshots += TransactionStateSnapshot.parseFrom(e.getValue)
    }
    txSnapshots.result()
  }

  def loadTxStateSnapshotsWithStatus(height: Height, rdb: RDB, transactions: Seq[Transaction]): Seq[(StateSnapshot, TxMeta.Status)] =
    loadTxStateSnapshots(height, rdb).zip(transactions).map { case (s, tx) => PBSnapshots.fromProtobuf(s, tx.id(), height) }

  def loadBlock(height: Height, rdb: RDB): Option[Block] =
    for {
      meta  <- rdb.db.get(Keys.blockMetaAt(height))
      block <- createBlock(PBBlocks.vanilla(meta.getHeader), meta.signature.toByteStr, loadTransactions(height, rdb).map(_._2)).toOption
    } yield block

  def fromHistory[A](resource: DBResource, historyKey: Key[Seq[Int]], valueKey: Int => Key[A]): Option[A] =
    for {
      h <- resource.get(historyKey).headOption
    } yield resource.get(valueKey(h))

  def loadAssetDescription(resource: DBResource, asset: IssuedAsset): Option[AssetDescription] =
    for {
      pbStaticInfo       <- resource.get(Keys.assetStaticInfo(asset))
      (info, volumeInfo) <- fromHistory(resource, Keys.assetDetailsHistory(asset), Keys.assetDetails(asset))
      sponsorship = fromHistory(resource, Keys.sponsorshipHistory(asset), Keys.sponsorship(asset)).fold(0L)(_.minFee)
      script      = fromHistory(resource, Keys.assetScriptHistory(asset), Keys.assetScript(asset)).flatten
    } yield AssetDescription(
      pbStaticInfo.sourceId.toByteStr,
      PublicKey(pbStaticInfo.issuerPublicKey.toByteStr),
      info.name,
      info.description,
      pbStaticInfo.decimals,
      volumeInfo.isReissuable,
      volumeInfo.volume,
      info.lastUpdatedAt,
      script,
      sponsorship,
      pbStaticInfo.isNft,
      pbStaticInfo.sequenceInBlock,
      Height(pbStaticInfo.height)
    )

  def loadActiveLeases(rdb: RDB, fromHeight: Int, toHeight: Int): Map[ByteStr, LeaseDetails] = rdb.db.withResource { r =>
    (for {
      id              <- loadLeaseIds(r, fromHeight, toHeight, includeCancelled = false)
      maybeNewDetails <- fromHistory(r, Keys.leaseDetailsHistory(id), Keys.leaseDetails(id))
      newDetails      <- maybeNewDetails
      if newDetails.isActive
    } yield (id, newDetails)).toMap
  }

  def loadLeaseIds(resource: DBResource, fromHeight: Int, toHeight: Int, includeCancelled: Boolean): Set[ByteStr] = {
    val leaseIds = mutable.Set.empty[ByteStr]

    val iterator = resource.fullIterator

    @inline
    def keyInRange(): Boolean = {
      val actualKey = iterator.key()
      actualKey.startsWith(KeyTags.LeaseDetails.prefixBytes) && Ints.fromByteArray(actualKey.slice(2, 6)) <= toHeight
    }

    iterator.seek(KeyTags.LeaseDetails.prefixBytes ++ Ints.toByteArray(fromHeight))
    while (iterator.isValid && keyInRange()) {
      val leaseId = ByteStr(iterator.key().drop(6))
      if (includeCancelled || readLeaseDetails(iterator.value()).isActive)
        leaseIds += leaseId
      else
        leaseIds -= leaseId

      iterator.next()
    }

    leaseIds.toSet
  }

  object AddressId extends TaggedType[Long] {
    def fromByteArray(bs: Array[Byte]): Type = AddressId(Longs.fromByteArray(bs))
  }

  type AddressId = AddressId.Type

  implicit final class Ops(private val value: AddressId) extends AnyVal {
    def toByteArray: Array[Byte] = Longs.toByteArray(AddressId.raw(value))
  }

  implicit class LongExt(val l: Long) extends AnyVal {
    def toByteArray: Array[Byte] = Longs.toByteArray(l)
  }
}
