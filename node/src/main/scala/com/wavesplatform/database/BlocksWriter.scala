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
import com.wavesplatform.utils.ScorexLogging
import monix.execution.Scheduler
import org.iq80.leveldb.DB

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal

//noinspection ScalaStyle
// TODO: refactor, implement rollback
private[database] final class BlocksWriter(writableDB: DB) extends Closeable with ScorexLogging {
  private[this] val flushDelay: FiniteDuration = 3 seconds
  private[this] val flushMinSize: Long         = (sys.runtime.maxMemory() / 30) max (1 * 1024 * 1024)
  private[this] val scheduler                  = Scheduler.singleThread("blocks-writer", daemonic = false)

  private[this] val blocks       = TrieMap.empty[Height, Block]
  private[this] val transactions = TrieMap.empty[TransactionId, (Height, TxNum, Transaction)]

  private[this] val rwLock               = new ReentrantReadWriteLock()
  @volatile private[this] var lastOffset = 0L
  private[this] var closed               = false

  // Init
  scheduler.scheduleWithFixedDelay(flushDelay, flushDelay) {
    @noinline
    def calculateFlushableBlocksSize(): Long =
      blocks.valuesIterator.map(_.bytes().length.toLong).sum

    val blocksSize = calculateFlushableBlocksSize()
    log.info(s"Blocks size is ${blocksSize / 1024 / 1024} mb")
    if (blocksSize >= flushMinSize) flushBlocks()
  }
  sys.addShutdownHook(this.close())

  def writeBlock(height: Height, block: Block): Unit = {
    // No lock
    require(!closed, "Already closed")
    for ((tx, num) <- block.transactionData.zipWithIndex) transactions(TransactionId(tx.id())) = (height, TxNum @@ num.toShort, tx)
    blocks(height) = block
  }

  // TODO: Get block raw bytes etc
  def getBlock(height: Height, withTxs: Boolean = false): Block = {
    blocks.getOrElse(
      height, {
        optimisticLocked(writableDB.get(Keys.blockOffset(height))) {
          input =>
            val headerSize  = input.readInt()
            val headerBytes = new Array[Byte](headerSize)
            input.read(headerBytes)

            val transactions = if (withTxs) {
              val txCount = input.readInt()
              for (_ <- 1 to txCount) yield {
                val txSize  = input.readInt()
                val txBytes = new Array[Byte](txSize)
                input.read(txBytes)
                transaction.PBSignedTransaction.parseFrom(txBytes)
              }
            } else Nil

            val block = protobuf.block.PBBlock
              .parseFrom(headerBytes)
              .withTransactions(transactions)

            import com.wavesplatform.common.utils._
            PBBlocks.vanilla(block, unsafe = true).explicitGet()
        }
      }
    )
  }

  def getTransactionHN(id: TransactionId): (Height, TxNum) = // No lock
    transactions
      .get(id)
      .fold {
        val (_, height, num) = optimisticLocked(0)(_ => writableDB.get(Keys.transactionOffset(id)))
        (height, num)
      }(v => (v._1, v._2))

  def getTransaction(id: TransactionId): (Height, TxNum, Transaction) =
    transactions.getOrElse(
      id, {
        val optimisticOffset = Try(writableDB.get(Keys.transactionOffset(id)))
        optimisticLocked(optimisticOffset.get._1) { input =>
          val txSize  = input.readInt()
          val txBytes = new Array[Byte](txSize)
          input.read(txBytes)

          import com.wavesplatform.common.utils._
          val (_, height, num) = optimisticOffset.getOrElse(writableDB.get(Keys.transactionOffset(id)))
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

        writableDB.readWrite(rw =>
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
        // log.warn(s"Last height is $flushingLastHeight")
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
  private[this] def unlockedRead[T](offset: Long)(f: DataInputStream => T): T = {
    val fs = fileChannelRead()
    fs.skip(offset).ensuring(_ == offset)
    val ds = new DataInputStream(fs)
    try f(ds)
    finally ds.close()
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
  private[this] def optimisticLocked[T](offset: => Long)(f: DataInputStream => T): T = {
    try {
      val offsetV = offset
      require(offsetV < this.lastOffset)
      unlockedRead(offsetV)(f)
    } catch { case NonFatal(_) => lockedRead(offset)(f) }
  }
}
