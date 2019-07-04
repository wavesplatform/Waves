package com.wavesplatform.database

import java.io.{Closeable, RandomAccessFile}
import java.util.concurrent.locks.{Lock, ReentrantReadWriteLock}

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

// TODO: refactor, implement rollback
private[database] final class BlocksWriter(writableDB: DB) extends Closeable with ScorexLogging {
  private[this] val flushDelay: FiniteDuration = 30 seconds
  private[this] implicit val scheduler                       = Scheduler.singleThread("write-db", daemonic = false)

  private[this] val blocks       = TrieMap.empty[Height, Block]
  private[this] val transactions = TrieMap.empty[TransactionId, (Height, TxNum, Transaction)]

  private[this] val rwLock      = new ReentrantReadWriteLock()
  private[this] lazy val fileChannel = new RandomAccessFile("blocks", "rw")

  // Init
  scheduler.scheduleWithFixedDelay(flushDelay, flushDelay)(flushBlocks())
  sys.addShutdownHook(this.close _)

  def writeBlock(height: Height, block: Block): Unit = {
    // No lock
    for ((tx, num) <- block.transactionData.zipWithIndex) transactions(TransactionId(tx.id())) = (height, TxNum @@ num.toShort, tx)
    blocks(height) = block
  }

  // TODO: Get block raw bytes etc
  def getBlock(height: Height, withTxs: Boolean = false): Block = {
    blocks.getOrElse(
      height,
      locked(_.readLock()) {
        val offset = writableDB.get(Keys.blockOffset(height))
        fileChannel.seek(offset)
        val headerSize  = fileChannel.readInt()
        val headerBytes = new Array[Byte](headerSize)
        fileChannel.read(headerBytes)

        val transactions = if (withTxs) {
          val txCount = fileChannel.readInt()
          for (_ <- 1 to txCount) yield {
            val txSize  = fileChannel.readInt()
            val txBytes = new Array[Byte](txSize)
            fileChannel.read(txBytes)
            transaction.PBSignedTransaction.parseFrom(txBytes)
          }
        } else Nil

        val block = protobuf.block.PBBlock
          .parseFrom(headerBytes)
          .withTransactions(transactions)

        import com.wavesplatform.common.utils._
        PBBlocks.vanilla(block, unsafe = true).explicitGet()
      }
    )
  }

  def getTransactionHN(id: TransactionId): (Height, TxNum) = // No lock
    transactions
      .get(id)
      .fold {
        val (_, height, num) = locked(_.readLock())(writableDB.get(Keys.transactionOffset(id)))
        (height, num)
      }(v => (v._1, v._2))

  def getTransaction(id: TransactionId): (Height, TxNum, Transaction) =
    transactions.getOrElse(
      id,
      locked(_.readLock()) {
        val (offset, height, num) = writableDB.get(Keys.transactionOffset(id))
        fileChannel.seek(offset)
        val txSize  = fileChannel.readInt()
        val txBytes = new Array[Byte](txSize)
        fileChannel.read(txBytes)

        import com.wavesplatform.common.utils._
        (height, num, PBTransactions.vanilla(transaction.PBSignedTransaction.parseFrom(txBytes), unsafe = true).explicitGet())
      }
    )

  @noinline
  private[this] def flushBlocks(): Unit = {
    log.warn("Flushing blocks1")
    if (blocks.isEmpty) return

    locked(_.writeLock()) {
      log.warn("Flushing blocks2")

      val blocksToRemove = new ArrayBuffer[Height]()
      val txsToRemove    = new ArrayBuffer[TransactionId]()

      writableDB.readWrite(rw =>
        for ((height, block) <- blocks.toSeq.sortBy(_._1); headerBytes = PBBlocks.protobuf(block.copy(transactionData = Nil)).toByteArray) {
          val blockOffset = fileChannel.length()
          fileChannel.seek(blockOffset)
          fileChannel.writeInt(headerBytes.length)
          fileChannel.write(headerBytes)
          rw.put(Keys.blockOffset(height), blockOffset)

          fileChannel.writeInt(block.transactionData.length)
          for ((tx, num) <- block.transactionData.zipWithIndex; txBytes = PBTransactions.protobuf(tx).toByteArray;
               transactionId = TransactionId(tx.id())) {
            val txOffset = fileChannel.length().ensuring(_ == fileChannel.getFilePointer)
            fileChannel.writeInt(txBytes.length)
            fileChannel.write(txBytes)
            /* if (tx.isInstanceOf[TransferTransaction]) */
            rw.put(Keys.transactionOffset(transactionId), (txOffset, height, TxNum @@ num.toShort))
            txsToRemove += transactionId
          }
          blocksToRemove += height
      })

      this.blocks --= blocksToRemove
      this.transactions --= txsToRemove
    }
  }

  def close(): Unit = synchronized {
    import scala.concurrent.duration._
    if (!scheduler.isShutdown) {
      scheduler.shutdown()
      scheduler.awaitTermination(10 minutes)
      flushBlocks()
      fileChannel.close()
    }
  }

  @noinline
  private[this] def locked[T](lockF: ReentrantReadWriteLock => Lock)(f: => T): T = {
    val lock = lockF(rwLock)
    concurrent.blocking(lock.lock())
    try f
    finally lock.unlock()
  }
}
