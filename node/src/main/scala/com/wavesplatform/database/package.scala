package com.wavesplatform

import java.io.IOException
import java.nio.ByteBuffer
import java.util.{Map => JMap}

import com.google.common.base.Charsets.UTF_8
import com.google.common.io.ByteStreams.{newDataInput, newDataOutput}
import com.google.common.io.{ByteArrayDataInput, ByteArrayDataOutput}
import com.google.common.primitives.{Bytes, Ints, Shorts}
import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.{Block, BlockHeader, SignerData}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.crypto._
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.state._
import com.wavesplatform.transaction.{Transaction, TransactionParsers}
import com.wavesplatform.utils.CloseableIterator
import org.iq80.leveldb.{DB, ReadOptions, Snapshot}

import scala.util.Try
import scala.util.control.NonFatal

package object database {
  final type DBEntry = JMap.Entry[Array[Byte], Array[Byte]]

  implicit class ByteArrayDataOutputExt(val output: ByteArrayDataOutput) extends AnyVal {
    def writeByteStr(s: ByteStr) = {
      output.write(s.arr)
    }

    def writeBigInt(v: BigInt): Unit = {
      val b = v.toByteArray
      require(b.length <= Byte.MaxValue)
      output.writeByte(b.length)
      output.write(b)
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
    def readBigInt(): BigInt = {
      val len = input.readByte()
      val b   = new Array[Byte](len)
      input.readFully(b)
      BigInt(b)
    }

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

  def readTxIds(data: Array[Byte]): List[ByteStr] = Option(data).fold(List.empty[ByteStr]) { d =>
    val b   = ByteBuffer.wrap(d)
    val ids = List.newBuilder[ByteStr]

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

  def writeTxIds(ids: Seq[ByteStr]): Array[Byte] =
    ids
      .foldLeft(ByteBuffer.allocate(ids.map(_.arr.length + 1).sum)) {
        case (b, id) =>
          b.put(id.arr.length match {
              case crypto.DigestSize      => crypto.DigestSize.toByte
              case crypto.SignatureLength => crypto.SignatureLength.toByte
            })
            .put(id.arr)
      }
      .array()

  def readStrings(data: Array[Byte]): Seq[String] = Option(data).fold(Seq.empty[String]) { _ =>
    var i = 0
    val s = Seq.newBuilder[String]

    while (i < data.length) {
      val len = Shorts.fromByteArray(data.drop(i))
      s += new String(data, i + 2, len, UTF_8)
      i += (2 + len)
    }
    s.result()
  }

  def writeStrings(strings: Seq[String]): Array[Byte] =
    strings
      .foldLeft(ByteBuffer.allocate(strings.map(_.getBytes(UTF_8).length + 2).sum)) {
        case (b, s) =>
          val bytes = s.getBytes(UTF_8)
          b.putShort(bytes.length.toShort).put(bytes)
      }
      .array()

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

  def writeTransactionInfo(txInfo: (Int, Transaction)) = {
    val (h, tx) = txInfo
    val txBytes = tx.bytes()
    ByteBuffer.allocate(4 + txBytes.length).putInt(h).put(txBytes).array()
  }

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

  def readAssetInfo(data: Array[Byte]): AssetInfo = {
    val ndi     = newDataInput(data)
    val reissue = ndi.readBoolean()
    val volume  = ndi.readBigInt()
    AssetInfo(reissue, volume)
  }

  def writeAssetInfo(ai: AssetInfo): Array[Byte] = {
    val ndo = newDataOutput()
    ndo.writeBoolean(ai.isReissuable)
    ndo.writeBigInt(ai.volume)
    ndo.toByteArray
  }

  def writeBlockHeaderAndSize(data: (BlockHeader, Int)): Array[Byte] = {
    val (bh, size) = data

    val ndo = newDataOutput()

    ndo.writeInt(size)

    ndo.writeByte(bh.version)
    ndo.writeLong(bh.timestamp)
    ndo.writeByteStr(bh.reference)
    ndo.writeLong(bh.consensusData.baseTarget)
    ndo.writeByteStr(bh.consensusData.generationSignature)

    if (bh.version == 1 | bh.version == 2)
      ndo.writeByte(bh.transactionCount)
    else
      ndo.writeInt(bh.transactionCount)

    ndo.writeInt(bh.featureVotes.size)
    bh.featureVotes.foreach(s => ndo.writeShort(s))
    ndo.write(bh.signerData.generator)
    ndo.writeByteStr(bh.signerData.signature)

    ndo.toByteArray
  }

  def readBlockHeaderAndSize(bs: Array[Byte]): (BlockHeader, Int) = {
    val ndi = newDataInput(bs)

    val size = ndi.readInt()

    val version    = ndi.readByte()
    val timestamp  = ndi.readLong()
    val reference  = ndi.readSignature
    val baseTarget = ndi.readLong()
    val genSig     = ndi.readByteStr(Block.GeneratorSignatureLength)
    val transactionCount = {
      if (version == 1 || version == 2) ndi.readByte()
      else ndi.readInt()
    }
    val featureVotesCount = ndi.readInt()
    val featureVotes      = List.fill(featureVotesCount)(ndi.readShort()).toSet
    val generator         = ndi.readPublicKey
    val signature         = ndi.readSignature

    val header = new BlockHeader(timestamp,
                                 version,
                                 reference,
                                 SignerData(generator, signature),
                                 NxtLikeConsensusBlockData(baseTarget, genSig),
                                 transactionCount,
                                 featureVotes)

    (header, size)
  }

  def readTransactionHNSeqAndType(bs: Array[Byte]): (Height, Seq[(Byte, TxNum)]) = {
    val ndi          = newDataInput(bs)
    val height       = Height(ndi.readInt())
    val numSeqLength = ndi.readInt()

    (height, List.fill(numSeqLength) {
      val tp  = ndi.readByte()
      val num = TxNum(ndi.readShort())
      (tp, num)
    })
  }

  def writeTransactionHNSeqAndType(v: (Height, Seq[(Byte, TxNum)])): Array[Byte] = {
    val (height, numSeq) = v
    val numSeqLength     = numSeq.length

    val outputLength = 4 + 4 + numSeqLength * (4 + 1)
    val ndo          = newDataOutput(outputLength)

    ndo.writeInt(height)
    ndo.writeInt(numSeqLength)
    numSeq.foreach {
      case (tp, num) =>
        ndo.writeByte(tp)
        ndo.writeShort(num)
    }

    ndo.toByteArray
  }

  def readTransactionHN(bs: Array[Byte]): (Height, TxNum) = {
    val ndi = newDataInput(bs)
    val h   = Height(ndi.readInt())
    val num = TxNum(ndi.readShort())

    (h, num)
  }

  def writeTransactionHN(v: (Height, TxNum)): Array[Byte] = {
    val ndo = newDataOutput(8)

    val (h, num) = v

    ndo.writeInt(h)
    ndo.writeShort(num)

    ndo.toByteArray
  }

  private[database] class LocalDBContext(val db: DB) {
    private[this] var counter = 0
    private[this] var snapshotV: Snapshot = _
    private[this] var rw: RW = _
    private[this] var ro: ReadOnlyDB = _

    def snapshot(): Snapshot = {
      require(!isClosed)
      if (snapshotV == null) snapshotV = db.getSnapshot.ensuring(_ != null, "Snapshot is null")
      snapshotV
    }

    def readOnlyDB(): ReadOnlyDB = {
      require(!isClosed)

      if (rw != null) rw
      else if (ro != null) ro
      else {
        val snapshot = this.snapshot()
        ro = new ReadOnlyDB(db, new ReadOptions().snapshot(snapshot))
        ro
      }
    }

    def readWriteDB(): RW = {
      require(!isClosed)

      if (rw == null) {
        val snapshot = this.snapshot()
        val readOptions = new ReadOptions().snapshot(snapshot)
        val batch = db.createWriteBatch()
        rw = RW(db, readOptions, batch)
      }
      rw
    }

    def incCounter(): Unit = {
      counter += 1
    }

    //noinspection ScalaStyle
    def close(): Unit = {
      if (isClosed) return

      counter -= 1
      if (counter == 0) {
        Option(rw).foreach(rw => db.write(rw.batch))
        Option(snapshotV).foreach(_.close())
        ro = null
        rw = null
        snapshotV = null
      }
    }

    def isClosed: Boolean =
      counter <= 0
  }

  private[database] class SynchronizedDBContext(db: DB) extends LocalDBContext(db) {
    override def snapshot(): Snapshot = synchronized(super.snapshot())

    override def readOnlyDB(): ReadOnlyDB = synchronized(super.readOnlyDB())

    override def readWriteDB(): RW = synchronized(super.readWriteDB())

    override def incCounter(): Unit = synchronized(super.incCounter())

    override def close(): Unit = synchronized(super.close())

    override def isClosed: Boolean = synchronized(super.isClosed)
  }

  private[database] final class DBContextHolder(val db: DB) {
    private[this] val tlContexts = new ThreadLocal[LocalDBContext]

    def openContext(): LocalDBContext = {
      val context = Option(tlContexts.get()) match {
        case Some(localContext) =>
          localContext
        case None =>
          val context = new LocalDBContext(db)
          tlContexts.set(context)
          context
      }

      context.incCounter()
      context
    }

    def withContext[T](f: LocalDBContext => T): T = {
      val context = openContext()
      try {
        context.incCounter()
        f(context)
      } finally {
        val err = Try(context.close()).failed
        if (context.isClosed) tlContexts.remove()
        err.foreach(throw _)
      }
    }
  }

  implicit class EntryExt(val e: JMap.Entry[Array[Byte], Array[Byte]]) extends AnyVal {
    import com.wavesplatform.crypto.DigestSize
    def extractId(offset: Int = 2, length: Int = DigestSize): ByteStr = {
      val id = ByteStr(new Array[Byte](length))
      Array.copy(e.getKey, offset, id.arr, 0, length)
      id
    }
  }

  implicit class DBContextHolderExt(private val ch: DBContextHolder) extends AnyVal {
    def readOnlyStream[A](f: ReadOnlyDB => CloseableIterator[A]): CloseableIterator[A] = {
      val ctx = new SynchronizedDBContext(ch.db)
      ctx.incCounter()
      val iterator = f(ctx.readOnlyDB())
      CloseableIterator(iterator, { () =>
        iterator.close()
        ctx.close()
      })
    }

    def readOnly[A](f: ReadOnlyDB => A): A =
      ch.withContext { ctx =>
        val db = ctx.readOnlyDB()
        f(db)
      }

    /**
      * @note Runs operations in batch, so keep in mind, that previous changes don't appear lately in f
      */
    def readWrite[A](f: RW => A): A =
      ch.withContext(ctx => f(ctx.readWriteDB()))
  }

  implicit class DBExt(private val db: DB) extends AnyVal {
    def createContext(): DBContextHolder = new DBContextHolder(db)

    // Compatibility
    def readOnlyStream[A](f: ReadOnlyDB => CloseableIterator[A]): CloseableIterator[A] = createContext().readOnlyStream(f)

    def readOnly[A](f: ReadOnlyDB => A): A = createContext().readOnly(f)

    def readWrite[A](f: RW => A): A = createContext().readWrite(f)

    def get[A](key: Key[A]): A = key.parse(db.get(key.keyBytes))

    def iterateOver(prefix: Short)(f: DBEntry => Unit): Unit =
      iterateOver(Shorts.toByteArray(prefix))(f)

    def iterateOver(prefix: Array[Byte])(f: DBEntry => Unit): Unit = {
      val iterator = db.iterator()
      try {
        iterator.seek(prefix)
        while (iterator.hasNext && iterator.peekNext().getKey.startsWith(prefix)) f(iterator.next())
      } finally iterator.close()
    }

    def iterateOverStream(): CloseableIterator[DBEntry] = CloseableIterator.defer {
      import scala.collection.JavaConverters._
      val dbIter = db.iterator()
      CloseableIterator(
        dbIter.asScala,
        () => dbIter.close()
      )
    }

    def iterateOverStream(prefix: Array[Byte], suffix: Array[Byte] = Array.emptyByteArray): CloseableIterator[DBEntry] = CloseableIterator.defer {
      import scala.collection.JavaConverters._
      val dbIter = db.iterator()
      try {
        dbIter.seek(Bytes.concat(prefix, suffix))
        CloseableIterator(
          dbIter.asScala.takeWhile(_.getKey.startsWith(prefix)),
          () => dbIter.close()
        )
      } catch {
        case NonFatal(err) =>
          dbIter.close()
          throw new IOException("Couldn't create DB iterator", err)
      }
    }

    def iterateOverStream(prefixes: Iterable[Array[Byte]]): CloseableIterator[DBEntry] = CloseableIterator.defer {
      import scala.collection.JavaConverters._
      val dbIter = db.iterator()

      def createIter(prefix: ByteStr, next: Seq[ByteStr]): Iterator[DBEntry] = {
        dbIter.seek(prefix)
        dbIter.asScala.takeWhile(_.getKey.startsWith(prefix.arr)) ++ (next match {
          case x +: xs => createIter(x, xs)
          case Nil => Iterator.empty
        })
      }

      try {
        val sortedPrefixes = prefixes.toList.map(ByteStr(_)).sorted
        CloseableIterator(
          if (prefixes.isEmpty) Iterator.empty else createIter(sortedPrefixes.head, sortedPrefixes.tail),
          () => dbIter.close()
        )
      } catch {
        case NonFatal(err) =>
          dbIter.close()
          throw new IOException("Couldn't create DB iterator", err)
      }
    }
  }
}
