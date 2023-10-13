package com.wavesplatform

import com.google.common.base.Charsets.UTF_8
import com.google.common.io.ByteStreams.{newDataInput, newDataOutput}
import com.google.common.io.{ByteArrayDataInput, ByteArrayDataOutput}
import com.google.common.primitives.{Bytes, Ints, Longs}
import com.google.protobuf.ByteString
import com.typesafe.scalalogging.Logger
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.block.validation.Validators
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.*
import com.wavesplatform.database.protobuf as pb
import com.wavesplatform.database.protobuf.DataEntry.Value
import com.wavesplatform.database.protobuf.TransactionData.Transaction as TD
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions}
import com.wavesplatform.state.*
import com.wavesplatform.state.StateHash.SectionId
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.{
  EthereumTransaction,
  GenesisTransaction,
  PBSince,
  PaymentTransaction,
  Transaction,
  TransactionParsers,
  TxValidationError
}
import com.wavesplatform.utils.*
import monix.eval.Task
import monix.reactive.Observable
import org.iq80.leveldb.*
import org.slf4j.LoggerFactory
import supertagged.TaggedType

import java.io.File
import java.nio.ByteBuffer
import java.util.Map as JMap
import scala.collection.mutable

//noinspection UnstableApiUsage
package object database {
  private lazy val logger: Logger = Logger(LoggerFactory.getLogger(getClass.getName))

  def openDB(path: String, recreate: Boolean = false): DB = {
    logger.debug(s"Open DB at $path")
    val file = new File(path)
    val options = new Options()
      .createIfMissing(true)
      .paranoidChecks(true)

    if (recreate) {
      LevelDBFactory.factory.destroy(file, options)
    }

    file.getAbsoluteFile.getParentFile.mkdirs()
    LevelDBFactory.factory.open(file, options)
  }

  final type DBEntry = JMap.Entry[Array[Byte], Array[Byte]]

  implicit class ByteArrayDataOutputExt(val output: ByteArrayDataOutput) extends AnyVal {
    def writeByteStr(s: ByteStr): Unit = {
      output.write(s.arr)
    }

    def writeScriptOption(v: Option[Script]): Unit = {
      output.writeBoolean(v.isDefined)
      v.foreach { s =>
        val b = s.bytes().arr
        output.writeShort(b.length)
        output.write(b)
      }
    }
  }

  implicit class ByteArrayDataInputExt(val input: ByteArrayDataInput) extends AnyVal {
    def readScriptOption(): Option[Script] = {
      if (input.readBoolean()) {
        val len = input.readShort()
        val b   = new Array[Byte](len)
        input.readFully(b)
        Some(ScriptReader.fromBytes(b).explicitGet())
      } else None
    }

    def readBytes(len: Int): Array[Byte] = {
      val arr = new Array[Byte](len)
      input.readFully(arr)
      arr
    }

    def readByteStr(len: Int): ByteStr = {
      ByteStr(readBytes(len))
    }

    def readSignature: ByteStr   = readByteStr(SignatureLength)
    def readPublicKey: PublicKey = PublicKey(readBytes(KeyLength))
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

  def readTxIds(data: Array[Byte]): List[ByteStr] = Option(data).fold(List.empty[ByteStr]) { d =>
    val b   = ByteBuffer.wrap(d)
    val ids = List.newBuilder[ByteStr]

    while (b.remaining() > 0) {
      val buffer = (b.get(): @unchecked) match {
        case crypto.DigestLength    => new Array[Byte](crypto.DigestLength)
        case crypto.SignatureLength => new Array[Byte](crypto.SignatureLength)
      }
      b.get(buffer)
      ids += ByteStr(buffer)
    }

    ids.result()
  }

  def writeTxIds(ids: Seq[ByteStr]): Array[Byte] =
    ids
      .foldLeft(ByteBuffer.allocate(ids.map(_.arr.length + 1).sum)) { case (b, id) =>
        b.put((id.arr.length: @unchecked) match {
          case crypto.DigestLength    => crypto.DigestLength.toByte
          case crypto.SignatureLength => crypto.SignatureLength.toByte
        }).put(id.arr)
      }
      .array()

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

  def writeLeaseBalance(lb: LeaseBalance): Array[Byte] = {
    val ndo = newDataOutput()
    ndo.writeLong(lb.in)
    ndo.writeLong(lb.out)
    ndo.toByteArray
  }

  def readLeaseBalance(data: Array[Byte]): LeaseBalance = Option(data).fold(LeaseBalance.empty) { d =>
    val ndi = newDataInput(d)
    LeaseBalance(ndi.readLong(), ndi.readLong())
  }

  def writeLeaseDetails(lde: Either[Boolean, LeaseDetails]): Array[Byte] =
    lde.fold(
      _ => throw new IllegalArgumentException("Can not write boolean flag instead of LeaseDetails"),
      ld =>
        pb.LeaseDetails(
          ByteString.copyFrom(ld.sender.arr),
          Some(PBRecipients.create(ld.recipient)),
          ld.amount,
          ByteString.copyFrom(ld.sourceId.arr),
          ld.height,
          ld.status match {
            case LeaseDetails.Status.Active => pb.LeaseDetails.Status.Active(com.google.protobuf.empty.Empty())
            case LeaseDetails.Status.Cancelled(height, cancelTxId) =>
              pb.LeaseDetails.Status
                .Cancelled(pb.LeaseDetails.Cancelled(height, cancelTxId.fold(ByteString.EMPTY)(id => ByteString.copyFrom(id.arr))))
            case LeaseDetails.Status.Expired(height) => pb.LeaseDetails.Status.Expired(pb.LeaseDetails.Expired(height))
          }
        ).toByteArray
    )

  def readLeaseDetails(data: Array[Byte]): Either[Boolean, LeaseDetails] =
    if (data.length == 1) Left(data(0) == 1)
    else {
      val d = pb.LeaseDetails.parseFrom(data)
      Right(
        LeaseDetails(
          d.senderPublicKey.toPublicKey,
          PBRecipients.toAddressOrAlias(d.recipient.get, AddressScheme.current.chainId).explicitGet(),
          d.amount,
          d.status match {
            case pb.LeaseDetails.Status.Active(_)                                   => LeaseDetails.Status.Active
            case pb.LeaseDetails.Status.Expired(pb.LeaseDetails.Expired(height, _)) => LeaseDetails.Status.Expired(height)
            case pb.LeaseDetails.Status.Cancelled(pb.LeaseDetails.Cancelled(height, transactionId, _)) =>
              LeaseDetails.Status.Cancelled(height, Some(transactionId.toByteStr).filter(!_.isEmpty))
            case pb.LeaseDetails.Status.Empty => ???
          },
          d.sourceId.toByteStr,
          d.height
        )
      )
    }

  def readVolumeAndFee(data: Array[Byte]): VolumeAndFee = Option(data).fold(VolumeAndFee.empty) { d =>
    val ndi = newDataInput(d)
    VolumeAndFee(ndi.readLong(), ndi.readLong())
  }

  def writeVolumeAndFee(vf: VolumeAndFee): Array[Byte] = {
    val ndo = newDataOutput()
    ndo.writeLong(vf.volume)
    ndo.writeLong(vf.fee)
    ndo.toByteArray
  }

  def readTransactionInfo(data: Array[Byte]): (Int, Transaction) =
    (Ints.fromByteArray(data), TransactionParsers.parseBytes(data.drop(4)).get)

  def readTransactionHeight(data: Array[Byte]): Int = Ints.fromByteArray(data)

  def readTransactionIds(data: Array[Byte]): Seq[(Int, ByteStr)] = Option(data).fold(Seq.empty[(Int, ByteStr)]) { d =>
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

  def writeTransactionIds(ids: Seq[(Int, ByteStr)]): Array[Byte] = {
    val size   = ids.foldLeft(0) { case (prev, (_, id)) => prev + 2 + id.arr.length }
    val buffer = ByteBuffer.allocate(size)
    for ((typeId, id) <- ids) {
      buffer.put(typeId.toByte).put(id.arr.length.toByte).put(id.arr)
    }
    buffer.array()
  }

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

  def writeBlockMeta(data: BlockMeta): Array[Byte] =
    pb.BlockMeta(
      Some(PBBlocks.protobuf(data.header)),
      ByteString.copyFrom(data.signature.arr),
      data.headerHash.fold(ByteString.EMPTY)(hh => ByteString.copyFrom(hh.arr)),
      data.height,
      data.size,
      data.transactionCount,
      data.totalFeeInWaves,
      data.reward.getOrElse(-1L),
      data.vrf.fold(ByteString.EMPTY)(vrf => ByteString.copyFrom(vrf.arr))
    ).toByteArray

  def readBlockMeta(bs: Array[Byte]): BlockMeta = {
    val pbbm = pb.BlockMeta.parseFrom(bs)
    BlockMeta(
      PBBlocks.vanilla(pbbm.header.get),
      pbbm.signature.toByteStr,
      Option(pbbm.headerHash).collect { case bs if !bs.isEmpty => bs.toByteStr },
      pbbm.height,
      pbbm.size,
      pbbm.transactionCount,
      pbbm.totalFeeInWaves,
      Option(pbbm.reward).filter(_ >= 0),
      Seq.empty,
      Option(pbbm.vrf).collect { case bs if !bs.isEmpty => bs.toByteStr }
    )
  }

  def readTransactionHNSeqAndType(bs: Array[Byte]): (Height, Seq[(Byte, TxNum)]) = {
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

  def writeTransactionHNSeqAndType(v: (Height, Seq[(Byte, TxNum)])): Array[Byte] = {
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

  def readDataEntry(key: String)(bs: Array[Byte]): DataEntry[?] =
    pb.DataEntry.parseFrom(bs).value match {
      case Value.Empty              => EmptyDataEntry(key)
      case Value.IntValue(value)    => IntegerDataEntry(key, value)
      case Value.BoolValue(value)   => BooleanDataEntry(key, value)
      case Value.BinaryValue(value) => BinaryDataEntry(key, value.toByteStr)
      case Value.StringValue(value) => StringDataEntry(key, value)
    }

  def writeDataEntry(e: DataEntry[?]): Array[Byte] =
    pb.DataEntry(e match {
      case IntegerDataEntry(_, value) => pb.DataEntry.Value.IntValue(value)
      case BooleanDataEntry(_, value) => pb.DataEntry.Value.BoolValue(value)
      case BinaryDataEntry(_, value)  => pb.DataEntry.Value.BinaryValue(ByteString.copyFrom(value.arr))
      case StringDataEntry(_, value)  => pb.DataEntry.Value.StringValue(value)
      case _: EmptyDataEntry          => pb.DataEntry.Value.Empty
    }).toByteArray

  implicit class EntryExt(val e: JMap.Entry[Array[Byte], Array[Byte]]) extends AnyVal {
    import com.wavesplatform.crypto.DigestLength
    def extractId(offset: Int = 2, length: Int = DigestLength): ByteStr = {
      val id = ByteStr(new Array[Byte](length))
      Array.copy(e.getKey, offset, id.arr, 0, length)
      id
    }
  }

  implicit class DBExt(val db: DB) extends AnyVal {
    def readOnly[A](f: ReadOnlyDB => A): A = {
      val snapshot = db.getSnapshot
      try f(new ReadOnlyDB(db, new ReadOptions().snapshot(snapshot)))
      finally snapshot.close()
    }

    /** @note
      *   Runs operations in batch, so keep in mind, that previous changes don't appear lately in f
      */
    def readWrite[A](f: RW => A): A = {
      val snapshot    = db.getSnapshot
      val readOptions = new ReadOptions().snapshot(snapshot)
      val batch       = new SortedBatch
      val rw          = new RW(db, readOptions, batch)
      val nativeBatch = db.createWriteBatch()
      try {
        val r = f(rw)
        batch.addedEntries.foreach { case (k, v) => nativeBatch.put(k.arr, v) }
        batch.deletedEntries.foreach(k => nativeBatch.delete(k.arr))
        db.write(nativeBatch, new WriteOptions().sync(false).snapshot(false))
        r
      } finally {
        nativeBatch.close()
        snapshot.close()
      }
    }

    def get[A](key: Key[A]): A                           = key.parse(db.get(key.keyBytes))
    def get[A](key: Key[A], readOptions: ReadOptions): A = key.parse(db.get(key.keyBytes, readOptions))
    def has(key: Key[?]): Boolean                        = db.get(key.keyBytes) != null

    def iterateOver(tag: KeyTags.KeyTag)(f: DBEntry => Unit): Unit = iterateOver(tag.prefixBytes)(f)

    def iterateOver(prefix: Array[Byte], seekPrefix: Array[Byte] = Array.emptyByteArray)(f: DBEntry => Unit): Unit = {
      val iterator = db.iterator()
      try {
        iterator.seek(Bytes.concat(prefix, seekPrefix))
        while (iterator.hasNext && iterator.peekNext().getKey.startsWith(prefix)) f(iterator.next())
      } finally iterator.close()
    }

    def resourceObservable: Observable[DBResource] = Observable.resource(Task(DBResource(db)))(r => Task(r.close()))

    def withResource[A](f: DBResource => A): A = {
      val resource = DBResource(db)
      try f(resource)
      finally resource.close()
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

  def readAccountScriptInfo(b: Array[Byte]): AccountScriptInfo = {
    val asi = pb.AccountScriptInfo.parseFrom(b)
    AccountScriptInfo(
      PublicKey(asi.publicKey.toByteArray),
      ScriptReader.fromBytes(asi.scriptBytes.toByteArray).explicitGet(),
      asi.maxComplexity,
      asi.callableComplexity.map { c =>
        c.version -> c.callableComplexity
      }.toMap
    )
  }

  def readTransaction(height: Height)(b: Array[Byte]): (TxMeta, Transaction) = {
    val data = pb.TransactionData.parseFrom(b)
    TxMeta(height, !data.failed, data.spentComplexity) -> toVanillaTransaction(data.transaction)
  }

  def toVanillaTransaction(tx: pb.TransactionData.Transaction): Transaction = tx match {
    case tx: TD.LegacyBytes         => TransactionParsers.parseBytes(tx.value.toByteArray).get
    case tx: TD.WavesTransaction    => PBTransactions.vanilla(tx.value, unsafe = false).explicitGet()
    case tx: TD.EthereumTransaction => EthereumTransaction(tx.value.toByteArray).explicitGet()
    case _                          => throw new IllegalArgumentException("Illegal transaction data")
  }

  def writeTransaction(v: (TxMeta, Transaction)): Array[Byte] = {
    val (m, tx) = v
    pb.TransactionData(toPbTransaction(tx), !m.succeeded, m.spentComplexity).toByteArray
  }

  def toPbTransaction(tx: Transaction): TD = tx match {
    case lps: PBSince if !lps.isProtobufVersion => TD.LegacyBytes(ByteString.copyFrom(tx.bytes()))
    case _: GenesisTransaction                  => TD.LegacyBytes(ByteString.copyFrom(tx.bytes()))
    case _: PaymentTransaction                  => TD.LegacyBytes(ByteString.copyFrom(tx.bytes()))
    case et: EthereumTransaction                => TD.EthereumTransaction(ByteString.copyFrom(et.bytes()))
    case _                                      => TD.WavesTransaction(PBTransactions.protobuf(tx))
  }

  def loadTransactions(height: Height, db: ReadOnlyDB): Seq[(TxMeta, Transaction)] = {
    val transactions = Seq.newBuilder[(TxMeta, Transaction)]
    db.iterateOver(KeyTags.NthTransactionInfoAtHeight.prefixBytes ++ Ints.toByteArray(height)) { e =>
      transactions += readTransaction(height)(e.getValue)
    }
    transactions.result()
  }

  def loadBlock(height: Height, db: ReadOnlyDB): Option[Block] =
    for {
      meta  <- db.get(Keys.blockMetaAt(height))
      block <- createBlock(meta.header, meta.signature, loadTransactions(height, db).map(_._2)).toOption
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

  def loadActiveLeases(db: DB, fromHeight: Int, toHeight: Int): Seq[LeaseTransaction] = db.withResource { r =>
    (for {
      id      <- loadLeaseIds(r, fromHeight, toHeight, includeCancelled = false)
      details <- fromHistory(r, Keys.leaseDetailsHistory(id), Keys.leaseDetails(id))
      if details.exists(_.fold(identity, _.isActive))
      tm <- r.get(Keys.transactionMetaById(TransactionId(id)))
      tx <- r.get(Keys.transactionAt(Height(tm.height), TxNum(tm.num.toShort)))
    } yield tx).collect {
      case (ltm, lt: LeaseTransaction) if ltm.succeeded => lt
    }.toSeq
  }

  def loadLeaseIds(resource: DBResource, fromHeight: Int, toHeight: Int, includeCancelled: Boolean): Set[ByteStr] = {
    val leaseIds = mutable.Set.empty[ByteStr]
    val iterator = resource.iterator

    @inline
    def keyInRange(): Boolean = {
      val actualKey = iterator.peekNext().getKey
      actualKey.startsWith(KeyTags.LeaseDetails.prefixBytes) && Ints.fromByteArray(actualKey.slice(2, 6)) <= toHeight
    }

    iterator.seek(KeyTags.LeaseDetails.prefixBytes ++ Ints.toByteArray(fromHeight))
    while (iterator.hasNext && keyInRange()) {
      val e       = iterator.next()
      val leaseId = ByteStr(e.getKey.drop(6))
      if (includeCancelled || readLeaseDetails(e.getValue).fold(identity, _.isActive))
        leaseIds += leaseId
      else
        leaseIds -= leaseId
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
