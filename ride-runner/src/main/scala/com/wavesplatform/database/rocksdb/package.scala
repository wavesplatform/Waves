package com.wavesplatform.database

import com.google.common.base.Charsets.UTF_8
import com.google.common.collect.{Interners, Maps}
import com.google.common.io.ByteStreams.{newDataInput, newDataOutput}
import com.google.common.io.{ByteArrayDataInput, ByteArrayDataOutput}
import com.google.common.primitives.{Bytes, Ints, Longs}
import com.google.protobuf.ByteString
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.protobuf as pb
import com.wavesplatform.database.protobuf.DataEntry.Value
import com.wavesplatform.lang.script.ScriptReader
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.protobuf.{ByteStrExt, ByteStringExt}
import com.wavesplatform.state.*
import com.wavesplatform.transaction
import com.wavesplatform.transaction.TxPositiveAmount
import com.wavesplatform.utils.*
import monix.eval.Task
import monix.reactive.Observable
import org.rocksdb.*
import sun.nio.ch.Util

import java.nio.ByteBuffer
import java.util
import java.util.Map as JMap
import scala.annotation.tailrec
import scala.collection.View
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import scala.util.Using

//noinspection UnstableApiUsage
package object rocksdb {
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

  def writeLeaseDetails(lde: Either[Boolean, LeaseDetails]): Array[Byte] =
    lde.fold(
      _ => throw new IllegalArgumentException("Can not write boolean flag instead of LeaseDetails"),
      ld =>
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
    )

  def readLeaseDetails(data: Array[Byte]): Either[Boolean, LeaseDetails] =
    if (data.length == 1) Left(data(0) == 1)
    else {
      val d = pb.LeaseDetails.parseFrom(data)
      Right(
        LeaseDetails(
          LeaseStaticInfo(
          d.senderPublicKey.toPublicKey,
          PBRecipients.toAddress(d.recipient.get, AddressScheme.current.chainId).explicitGet(),
          TxPositiveAmount.unsafeFrom(d.amount),
            d.sourceId.toByteStr,
            d.height
          ),
          d.cancelReason match {
            case pb.LeaseDetails.CancelReason.Empty                                   => LeaseDetails.Status.Active
            case pb.LeaseDetails.CancelReason.Expired(pb.LeaseDetails.Expired(height, _)) => LeaseDetails.Status.Expired(height)
            case pb.LeaseDetails.CancelReason.Cancelled(pb.LeaseDetails.Cancelled(height, transactionId, _)) =>
              LeaseDetails.Status.Cancelled(height, Some(transactionId.toByteStr).filter(!_.isEmpty))
          }

        )
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

  def oldReadTransactionHNSeqAndType(bs: Array[Byte]): (Height, Seq[(Byte, TxNum)]) = {
    val ndi          = newDataInput(bs)
    val height       = Height(ndi.readInt())
    val numSeqLength = ndi.readInt()

    (
      height,
      List.fill(numSeqLength) {
        val tp  = ndi.readByte()
        val num = TxNum(ndi.readShort())
        (tp, num)
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

  def oldWriteTransactionHNSeqAndType(v: (Height, Seq[(Byte, TxNum)])): Array[Byte] = {
    val (height, numSeq) = v
    val numSeqLength     = numSeq.length

    val outputLength = 4 + 4 + numSeqLength * (4 + 1)
    val ndo          = newDataOutput(outputLength)

    ndo.writeInt(height)
    ndo.writeInt(numSeqLength)
    numSeq.foreach { case (tp, num) =>
      ndo.writeByte(tp)
      ndo.writeShort(num)
    }

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
      }((resource: Snapshot) => db.releaseSnapshot(resource))
    }

    /** @note
      *   Runs operations in batch, so keep in mind, that previous changes don't appear lately in f
      */
    def readWrite[A](f: RW => A): A = {
      Using.resource(db.getSnapshot) { s =>
        Using.resource(new ReadOptions().setSnapshot(s).setVerifyChecksums(false)) { ro =>
          Using.resource(new WriteOptions().setSync(false).setDisableWAL(true)) { wo =>
            Using.resource(new WriteBatch()) { wb =>
              val r = f(new RW(db, ro, wb))
              db.write(wo, wb)
              r
            }
          }
        }
      } { (resource: Snapshot) =>
        db.releaseSnapshot(resource)
        resource.close()
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

      val result = keys.view
        .zip(db.multiGetByteBuffers(readOptions, keyBufs, valBufs).asScala)
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

    def multiGetInts(readOptions: ReadOptions, keys: Seq[Array[Byte]]): Seq[Option[Int]] = {
      val keyBufs = getKeyBuffers(keys)
      val valBufs = getValueBuffers(keys.size, 4)

      val result = db
        .multiGetByteBuffers(readOptions, keyBufs, valBufs)
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

      val result = keys.view
        .zip(db.multiGetByteBuffers(readOptions, keyBufs, valBufs).asScala)
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
    def get[A](key: Key[A], readOptions: ReadOptions): A = key.parse(db.get(readOptions, key.keyBytes))
    def has(key: Key[?]): Boolean                        = db.get(key.keyBytes) != null

    def iterateOver(tag: KeyTags.KeyTag)(f: DBEntry => Unit): Unit = iterateOver(tag.prefixBytes)(f)

    def iterateOver(prefix: Array[Byte], seekPrefix: Array[Byte] = Array.emptyByteArray, cfh: Option[ColumnFamilyHandle] = None)(
        f: DBEntry => Unit
    ): Unit = {
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
        iterator.seek(Bytes.concat(prefix, seekPrefix))
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
      val result = keys.view
        .zip(db.multiGetByteBuffers(readOptions, keyBufs, valBufs).asScala)
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
      val result = keys.view
        .zip(db.multiGetByteBuffers(readOptions, keyBufs, valBufs).asScala)
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

  def writeAssetScript(script: AssetScriptInfo): Array[Byte] =
    Longs.toByteArray(script.complexity) ++ script.script.bytes().arr

  def readAssetScript(b: Array[Byte]): AssetScriptInfo =
    AssetScriptInfo(ScriptReader.fromBytes(b.drop(8)).explicitGet(), Longs.fromByteArray(b))

  def writeAccountScriptInfo(scriptInfo: AccountScriptInfo): Array[Byte] =
    pb.AccountScriptInfo.toByteArray(
      pb.AccountScriptInfo(
        scriptInfo.publicKey.toByteString,
        scriptInfo.script.bytes().toByteString,
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
        asi.publicKey.toPublicKey,
        ScriptReader.fromBytes(asi.scriptBytes.toByteArray).explicitGet(),
        asi.maxComplexity,
        asi.callableComplexity.map { c =>
          c.version -> c.callableComplexity
        }.toMap
      )
    )
  }

  implicit final class Ops(private val value: AddressId) extends AnyVal {
    def toByteArray: Array[Byte] = Longs.toByteArray(AddressId.raw(value))
  }

  implicit class LongExt(val l: Long) extends AnyVal {
    def toByteArray: Array[Byte] = Longs.toByteArray(l)
  }
}
