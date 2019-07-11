package com.wavesplatform.database

import java.io._
import java.util.concurrent.locks.{Lock, ReentrantReadWriteLock}

import com.google.common.primitives.Ints
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils._
import com.wavesplatform.protobuf
import com.wavesplatform.protobuf.block.{PBBlock, PBBlocks}
import com.wavesplatform.protobuf.transaction
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions, VanillaTransaction}
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.state.{Height, TransactionId, TxNum}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.{CloseableIterator, ScorexLogging}
import monix.execution.Scheduler

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

//noinspection ScalaStyle
// TODO: refactor, implement rollback
private[database] final class BlocksWriter(dbContext: DBContextHolder) extends Closeable with ScorexLogging {
  private[this] val flushDelay: FiniteDuration = 3 seconds // TODO: add force flush delay
  private[this] val flushMinSize: Long = (sys.runtime.maxMemory() / 50) max (1 * 1024 * 1024)
  private[this] val scheduler                  = Scheduler.singleThread("blocks-writer", daemonic = false)

  private[this] val blocks = TrieMap.empty[Height, PBBlock]
  private[this] val transactions = TrieMap.empty[TransactionId, (Height, TxNum, PBSignedTransaction)]

  private[this] val rwLock = new ReentrantReadWriteLock()

  @volatile
  private[this] var lastOffset =
    Try(dbContext.readOnly { db =>
      val h = db.get(Keys.height)
      db.get(Keys.blockOffset(h))
    }).getOrElse(0L)

  @volatile
  private[this] var closed = false

  // Init
  scheduler.scheduleWithFixedDelay(flushDelay, flushDelay) {

    val blocksSize = calculateFlushableBlocksSize()
    // log.info(s"Blocks size is ${blocksSize / 1024 / 1024} mb")
    if (blocksSize >= flushMinSize) flushBlocks()
  }
  sys.addShutdownHook(this.close())

  private[this] def readBlockFrom(input: DataInputStream, withTxs: Boolean): (PBBlock, Int) = {
    val headerSize  = input.readInt()
    val headerBytes = new Array[Byte](headerSize)
    input.read(headerBytes)

    var allTxsSize = 0
    val transactions = if (withTxs) {
      val txCount = input.readInt()
      for (_ <- 1 to txCount) yield {
        val txSize  = input.readInt()
        val txBytes = new Array[Byte](txSize)
        input.read(txBytes)
        allTxsSize += Ints.BYTES + txSize
        transaction.PBSignedTransaction.parseFrom(txBytes)
      }
    } else Nil

    val protoBlock = protobuf.block.PBBlock
      .parseFrom(headerBytes)
      .withTransactions(transactions)

    val size = Ints.BYTES + headerBytes.length + Ints.BYTES + allTxsSize
    (protoBlock, size)
  }

  private[this] def readBlockAt(offset: Long, withTxs: Boolean): (PBBlock, Int) = {
    optimisticRead(offset)(readBlockFrom(_, withTxs))
  }

  def writeBlock(height: Height, block: Block): Unit = {
    // No lock
    require(!closed, "Already closed")

    val protoBlock = PBBlocks.protobuf(block)
    for (((tx, vtx), num) <- protoBlock.transactions.zip(block.transactionData).zipWithIndex)
      transactions(TransactionId(vtx.id())) = (height, TxNum @@ num.toShort, tx)

    blocks(height) = protoBlock
  }

  // Not really safe
  def blocksIterator(): CloseableIterator[Block] = Try(dbContext.db.get(Keys.blockOffset(1))) match {
    case Success(startOffset) =>
      val (inMemBlocks, endOffset) = locked(_.readLock()) {
        (this.blocks.toVector.sortBy(_._1).map(_._2), this.lastOffset)
      }

      unlockedRead(startOffset, close = false) { input =>
        def createIter(offset: Long): Iterator[PBBlock] = {
          if (offset <= endOffset) Try(readBlockFrom(input, withTxs = true)) match {
            case Success((block, size)) => Iterator.single(block) ++ createIter(offset + size)
            case Failure(err)           => throw new IOException("Failed to create blocks iterator", err)
          } else Iterator.empty
        }

        CloseableIterator(
          createIter(startOffset) ++ inMemBlocks,
          () => input.close()
        ).map(PBBlocks.vanilla(_, unsafe = true).explicitGet())
      }

    case Failure(_) =>
      Iterator.empty
  }

  def deleteBlock(h: Height): Unit =
    lockedWrite { (_, _) =>
      dbContext.readWrite { rw =>
        Try(getBlock(h, withTxs = true))
          .fold(_ => Nil, _.transactionData)
          .foreach(tx => rw.delete(Keys.transactionOffset(TransactionId @@ tx.id())))

        rw.delete(Keys.blockOffset(h))
      }

      blocks.remove(h)
      transactions --= transactions.filter(_._2._1 == h).keys
    }

  // TODO: Get block raw bytes etc
  def getBlock(height: Height, withTxs: Boolean = false): Block = {
    val protoBlock = blocks
      .getOrElse(height, readBlockAt(dbContext.db.get(Keys.blockOffset(height)), withTxs)._1)
    PBBlocks.vanilla(protoBlock, unsafe = true).explicitGet()
  }

  def getTransactionHN(id: TransactionId): (Height, TxNum) = // No lock
    transactions
      .get(id)
      .fold {
        val (_, height, num) = optimisticRead(0)(_ => dbContext.db.get(Keys.transactionOffset(id)))
        (height, num)
      }(v => (v._1, v._2))

  def getTransactionsByHN(hn: (Height, TxNum)*): CloseableIterator[(Height, TxNum, Transaction)] = {
    val (inMemTxs, endOffset) = locked(_.readLock()) {
      val heightNumSet = hn.toSet
      (this.transactions.values
        .collect { case (h, n, tx) if heightNumSet.contains(h -> n) => (h, n, toTransaction(tx)) }
        .toVector
        .sortBy(v => (v._1, v._2)),
       this.lastOffset)
    }

    val fileTxs = dbContext.readOnlyStream(db =>
      unlockedRead(0L, close = false) { input =>
        var currentOffset = 0L

        val sorted = hn.groupBy(_._1).toSeq.sortBy(_._1)
        val iterator = sorted.iterator.flatMap {
          case (height, nums) =>
            Try(db.get(Keys.blockOffset(height))) match {
              case Success(offset) if offset >= currentOffset && offset <= endOffset =>
                currentOffset += input.skip(offset - currentOffset)
                val numsSet = nums.map(_._2.toInt).toSet

                def readNums(): Seq[(TxNum, PBSignedTransaction)] = {
                  val headerSize = input.readInt()
                  currentOffset += Ints.BYTES + input.skip(headerSize)

                  val txs = new ArrayBuffer[(TxNum, PBSignedTransaction)](numsSet.size)
                  val txCount = input.readInt()
                  currentOffset += Ints.BYTES

                  for (n <- 0 until txCount) yield {
                    val txSize = input.readInt()
                    currentOffset += Ints.BYTES

                    if (numsSet.contains(n)) {
                      val txBytes = new Array[Byte](txSize)
                      currentOffset += input.read(txBytes)
                      txs += (TxNum @@ n.toShort -> transaction.PBSignedTransaction.parseFrom(txBytes))
                    } else {
                      currentOffset += input.skip(txSize)
                    }
                  }

                  txs
                }

                readNums().map { case (num, tx) => (height, num, PBTransactions.vanilla(tx, unsafe = true).right.get) }

              case _ =>
                Nil
            }
        }

        CloseableIterator(iterator, () => input.close())
    })

    fileTxs ++ inMemTxs
  }

  def getTransaction(id: TransactionId): (Height, TxNum, Transaction) =
    transactions
      .get(id)
      .map { case (h, n, tx) => (h, n, toTransaction(tx)) }
      .getOrElse {
        val optimisticOffset = Try(dbContext.db.get(Keys.transactionOffset(id)))
        optimisticRead(optimisticOffset.get._1) { input =>
          val txSize  = input.readInt()
          val txBytes = new Array[Byte](txSize)
          input.read(txBytes)

          val (_, height, num) = optimisticOffset.getOrElse(dbContext.db.get(Keys.transactionOffset(id)))
          (height, num, toTransaction(txBytes))
        }
      }

  @noinline
  private[this] def flushBlocks(): Unit = {
    log.warn("Flushing blocks1")

    val removed = lockedWrite {
      case (offset, output) =>
        var currentOffset = offset
        log.warn("Flushing blocks2")

        val blocksToRemove = new ArrayBuffer[Height]()
        val txsToRemove    = new ArrayBuffer[TransactionId]()

        dbContext.readWrite(rw =>
          for ((height, protoBlock) <- blocks.toSeq.sortBy(_._1)) {
            this.lastOffset = currentOffset

            val headerBytes = PBUtils.encodeDeterministic(protoBlock.withTransactions(Nil))
            output.writeInt(headerBytes.length)
            output.write(headerBytes)
            rw.put(Keys.blockOffset(height), currentOffset)
            currentOffset += Ints.BYTES + headerBytes.length

            val transactions = protoBlock.transactions
            output.writeInt(transactions.length)
            currentOffset += Ints.BYTES
            for ((tx, num) <- transactions.zipWithIndex; vtx <- PBTransactions.vanilla(tx, unsafe = true); txBytes = PBUtils.encodeDeterministic(tx);
                 transactionId = TransactionId(vtx.id())) {
              output.writeInt(txBytes.length)
              output.write(txBytes)
              /* if (tx.isInstanceOf[TransferTransaction]) */
              rw.put(Keys.transactionOffset(transactionId), (currentOffset, height, TxNum @@ num.toShort))
              currentOffset += Ints.BYTES + txBytes.length
              txsToRemove += transactionId
            }
            blocksToRemove += height
            // log.info(s"block at $height is $block, offset is $offset")
        })

        val removed = calculateFlushableBlocksSize()
        this.blocks --= blocksToRemove
        this.transactions --= txsToRemove
        removed
    }
    log.warn("Flushing blocks3")
    // System.gc()

    log.info(f"${removed.toDouble / 1024 / 1024}%.2f Mb of blocks flushed")
    val size = calculateFlushableBlocksSize()
    if (size > 0) log.warn(f"${size.toDouble / 1024 / 1024}%.2f MB of blocks retained after flush")
  }

  def close(): Unit = synchronized {
    if (!this.closed) {
      this.closed = true
      scheduler.shutdown()
      scheduler.awaitTermination(5 minutes)
      while (blocks.nonEmpty) {
        val flushingLastHeight = blocks.keys.max
        log.warn(s"Last height is $flushingLastHeight")
        flushBlocks()
      }
    }
  }

  override def finalize(): Unit =
    this.close()

  @noinline
  private[this] def locked[T](lockF: ReentrantReadWriteLock => Lock)(f: => T): T = {
    val lock = lockF(rwLock)
    concurrent.blocking(lock.lock())
    try f
    finally lock.unlock()
  }

  private[this] def fileChannelWrite() = {
    new FileOutputStream("blocks", true)
  }

  private[this] def fileChannelRead() = {
    val f = new File("blocks")
    if (!f.exists()) f.createNewFile()
    new FileInputStream(f)
  }

  @noinline
  private[this] def unlockedRead[T](offset: Long, close: Boolean = true)(f: DataInputStream => T): T = {
    val fs = fileChannelRead()
    fs.skip(offset).ensuring(_ == offset)
    val ds = new DataInputStream(fs)
    try f(ds)
    finally if (close) ds.close()
  }

  @noinline
  private[this] def lockedRead[T](offset: Long)(f: DataInputStream => T): T =
    locked(_.readLock())(unlockedRead(offset)(f))

  @noinline
  private[this] def lockedWrite[T](f: (Long, DataOutputStream) => T): T = {
    locked(_.writeLock()) {
      val fs = fileChannelWrite()
      val ds = new DataOutputStream(fs)
      try f(fs.getChannel.position(), ds)
      finally ds.close()
    }
  }

  @noinline
  private[this] def optimisticRead[T](offset: => Long)(f: DataInputStream => T): T = {
    try {
      val offsetV = offset
      require(offsetV < this.lastOffset)
      unlockedRead(offsetV)(f)
    } catch { case NonFatal(_) => lockedRead(offset)(f) }
  }

  @inline
  private[this] def calculateFlushableBlocksSize(): Long =
    blocks.valuesIterator.map(_.serializedSize.toLong).sum

  private[this] def toTransaction(tx: PBSignedTransaction): VanillaTransaction = {
    import com.wavesplatform.common.utils._
    PBTransactions.vanilla(tx, unsafe = true).explicitGet()
  }

  private[this] def toTransaction(tx: Array[Byte]): VanillaTransaction =
    toTransaction(PBSignedTransaction.parseFrom(tx))
}
