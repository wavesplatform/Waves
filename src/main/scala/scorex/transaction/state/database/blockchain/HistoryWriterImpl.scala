package scorex.transaction.state.database.blockchain

import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.google.common.util.concurrent.UncheckedExecutionException
import scorex.account.Account
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.network.Checkpoint
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError.CustomError
import scorex.transaction.{CheckpointService, History, HistoryWriter, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.util.{Failure, Try}

class HistoryWriterImpl(storage: HistoryStorage) extends History with HistoryWriter with CheckpointService with ScorexLogging {

  private val BlocksCacheSizeLimit: Int = 1000
  private val blocksCache = CacheBuilder.newBuilder()
    .maximumSize(BlocksCacheSizeLimit)
    .build[Integer, Block](
    new CacheLoader[Integer, Block]() {
      def load(height: Integer): Block = {
        val blockOpt = Try(Option(storage.blockBodyByHeight.get(height))).toOption.flatten.flatMap(b => Block.parseBytes(b).recoverWith {
          case t: Throwable =>
            log.error("Block.parseBytes error", t)
            Failure(t)
        }.toOption)
        blockOpt.get
      }
    })

  override def appendBlock(block: Block): Either[ValidationError, Unit] = {
    if ((height() == 0) || (this.lastBlock.uniqueId sameElements block.reference)) {
      val h = height() + 1
      storage.blockBodyByHeight.put(h, block.bytes)
      storage.scoreByHeight.put(h, score() + block.blockScore)
      storage.blockIdByHeight.put(h, block.uniqueId)
      storage.heightByBlockId.put(block.uniqueId, h)
      Right(())
    } else {
      Left(CustomError(s"Appending block ${block.json} which parent is not last block in blockchain"))
    }
  }

  override def discardBlock(): Unit = {
    require(height() > 1, "Chain is empty or contains genesis block only, can't make rollback")
    val h = height()
    blocksCache.invalidate(h)
    storage.blockBodyByHeight.remove(h)
    val vOpt = Option(storage.blockIdByHeight.remove(h))
    vOpt.map(v => storage.heightByBlockId.remove(v))
  }

  override def blockAt(height: Int): Option[Block] = {
    try {
      Some(blocksCache.get(height))
    } catch {
      case e: UncheckedExecutionException =>
        log.debug(s"There are no block at $height")
        None
    }
  }

  override def lastBlockIds(howMany: Int): Seq[BlockId] =
    (Math.max(1, height() - howMany + 1) to height()).flatMap(i => Option(storage.blockIdByHeight.get(i)))
      .reverse

  override def height(): Int = storage.blockIdByHeight.size()

  override def score(): BlockchainScore = if (height() > 0) storage.scoreByHeight.get(height()) else 0

  override def scoreOf(id: BlockId): BlockchainScore = heightOf(id).map(storage.scoreByHeight.get(_)).getOrElse(0)

  override def heightOf(blockSignature: Array[Byte]): Option[Int] = Option(storage.heightByBlockId.get(blockSignature))

  override def blockById(blockId: BlockId): Option[Block] = heightOf(blockId).flatMap(blockAt)

  override def generatedBy(account: Account, from: Int, to: Int): Seq[Block] = {
    (from to to).toStream.flatMap { h =>
      for {
        block <- blockAt(h)
        if block.signerData.generator.address.equals(account.address)
      } yield block
    }
  }

  override def toString: String = ((1 to height()) map { h =>
    val bl = blockAt(h).get
    s"$h -- ${bl.uniqueId.mkString} -- ${bl.referenceField.value.mkString}"
  }).mkString("\n")

  override def getCheckpoint: Option[Checkpoint] = Option(storage.checkpoint.get(0))

  override def setCheckpoint(c: Option[Checkpoint]): Unit = {
    if (c.isDefined) storage.checkpoint.put(0, c.get)
    else storage.checkpoint.remove(0)
  }
}
