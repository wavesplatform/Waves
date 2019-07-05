package com.wavesplatform.database

import java.io._
import java.util.concurrent.locks.{Lock, ReentrantReadWriteLock}

import com.google.common.primitives.Ints
import com.wavesplatform.block.Block
import com.wavesplatform.protobuf
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.transaction
import com.wavesplatform.protobuf.transaction.PBTransactions
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
  private[this] val flushMinSize: Long         = (sys.runtime.maxMemory() / 30) max (1 * 1024 * 1024)
  private[this] val scheduler                  = Scheduler.singleThread("blocks-writer", daemonic = false)

  private[this] val blocks       = TrieMap.empty[Height, Block]
  private[this] val transactions = TrieMap.empty[TransactionId, (Height, TxNum, Transaction)]

  private[this] val rwLock = new ReentrantReadWriteLock()
  @volatile private[this] var lastOffset = Try(dbContext.readOnly { db =>
    val h = db.get(Keys.height)
    db.get(Keys.blockOffset(h))
  }).getOrElse(0L)
  private[this] var closed = false

  // Init
  scheduler.scheduleWithFixedDelay(flushDelay, flushDelay) {
    @noinline
    def calculateFlushableBlocksSize(): Long =
      blocks.valuesIterator.map(_.bytes().length.toLong).sum

    val blocksSize = calculateFlushableBlocksSize()
    // log.info(s"Blocks size is ${blocksSize / 1024 / 1024} mb")
    if (blocksSize >= flushMinSize) flushBlocks()
  }
  sys.addShutdownHook(this.close())

  private[this] def readBlockFrom(input: DataInputStream, withTxs: Boolean): (Block, Int) = {
    val headerSize = input.readInt()
    val headerBytes = new Array[Byte](headerSize)
    input.read(headerBytes)

    var allTxsSize = 0
    val transactions = if (withTxs) {
      val txCount = input.readInt()
      for (_ <- 1 to txCount) yield {
        val txSize = input.readInt()
        val txBytes = new Array[Byte](txSize)
        input.read(txBytes)
        allTxsSize += Ints.BYTES + txSize
        transaction.PBSignedTransaction.parseFrom(txBytes)
      }
    } else Nil

    val protoBlock = protobuf.block.PBBlock
      .parseFrom(headerBytes)
      .withTransactions(transactions)

    import com.wavesplatform.common.utils._
    val block = PBBlocks.vanilla(protoBlock, unsafe = true).explicitGet()
    val size = Ints.BYTES + headerBytes.length + Ints.BYTES + allTxsSize
    (block, size)
  }

  private[this] def readBlockAt(offset: Long, withTxs: Boolean): (Block, Int) = {
    optimisticRead(offset)(readBlockFrom(_, withTxs))
  }

  def writeBlock(height: Height, block: Block): Unit = {
    // No lock
    require(!closed, "Already closed")
    for ((tx, num) <- block.transactionData.zipWithIndex) transactions(TransactionId(tx.id())) = (height, TxNum @@ num.toShort, tx)
    blocks(height) = block
  }

  // Not really safe
  def blocksIterator(): CloseableIterator[Block] = Try(dbContext.db.get(Keys.blockOffset(1))) match {
    case Success(startOffset) =>
      val (inMemBlocks, endOffset) = locked(_.readLock()) {
        (this.blocks.toVector.sortBy(_._1).map(_._2), this.lastOffset)
      }

      unlockedRead(startOffset, close = false) { input =>
        def createIter(offset: Long): Iterator[Block] = {
          if (offset <= endOffset) Try(readBlockFrom(input, withTxs = true)) match {
            case Success((block, size)) => Iterator.single(block) ++ createIter(offset + size)
            case Failure(err) => throw new IOException("Failed to create blocks iterator", err)
          } else Iterator.empty
        }

        CloseableIterator(
          createIter(startOffset) ++ inMemBlocks,
          () => input.close()
        )
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

      blocks.remove(h) match {
        case Some(block) => this.transactions --= block.transactionData.map(tx => TransactionId(tx.id()))
        case None => // Ignore
      }
    }

  // TODO: Get block raw bytes etc
  def getBlock(height: Height, withTxs: Boolean = false): Block =
    blocks.getOrElse(height, readBlockAt(dbContext.db.get(Keys.blockOffset(height)), withTxs)._1)

  def getTransactionHN(id: TransactionId): (Height, TxNum) = // No lock
    transactions
      .get(id)
      .fold {
        val (_, height, num) = optimisticRead(0)(_ => dbContext.db.get(Keys.transactionOffset(id)))
        (height, num)
      }(v => (v._1, v._2))

  def getTransaction(id: TransactionId): (Height, TxNum, Transaction) =
    transactions.getOrElse(
      id, {
        val optimisticOffset = Try(dbContext.db.get(Keys.transactionOffset(id)))
        optimisticRead(optimisticOffset.get._1) { input =>
          val txSize  = input.readInt()
          val txBytes = new Array[Byte](txSize)
          input.read(txBytes)

          import com.wavesplatform.common.utils._
          val (_, height, num) = optimisticOffset.getOrElse(dbContext.db.get(Keys.transactionOffset(id)))
          (height, num, PBTransactions.vanilla(transaction.PBSignedTransaction.parseFrom(txBytes), unsafe = true).explicitGet())
        }
      }
    )

  @noinline
  private[this] def flushBlocks(): Unit = {
    log.warn("Flushing blocks1")

    lockedWrite {
      case (offset, output) =>
        var currentOffset = offset
        log.warn("Flushing blocks2")

        val blocksToRemove = new ArrayBuffer[Height]()
        val txsToRemove    = new ArrayBuffer[TransactionId]()

        dbContext.readWrite(rw =>
          for ((height, block) <- blocks.toSeq.sortBy(_._1); headerBytes = PBBlocks.protobuf(block.copy(transactionData = Nil)).toByteArray) {
            this.lastOffset = currentOffset

            output.writeInt(headerBytes.length)
            output.write(headerBytes)
            rw.put(Keys.blockOffset(height), currentOffset)
            currentOffset += Ints.BYTES + headerBytes.length

            output.writeInt(block.transactionData.length)
            currentOffset += Ints.BYTES
            for ((tx, num) <- block.transactionData.zipWithIndex; txBytes = PBTransactions.protobuf(tx).toByteArray;
                 transactionId = TransactionId(tx.id())) {
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

        this.blocks --= blocksToRemove
        this.transactions --= txsToRemove
    }
    log.warn("Flushing blocks3")
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
}
