package scorex.transaction.state.database.blockchain

import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.google.common.util.concurrent.UncheckedExecutionException
import org.h2.mvstore.{MVMap, MVStore}
import scorex.account.Account
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.network.Checkpoint
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError.CustomError
import scorex.transaction.{History, ValidationError}
import scorex.utils.{LogMVMapBuilder, ScorexLogging}

import scala.collection.JavaConverters._
import scala.util.{Failure, Try}

class StoredBlockchain(db: MVStore) extends History with ScorexLogging {

  case class BlockchainPersistence(database: MVStore) {
    val blocks: MVMap[Int, Array[Byte]] = database.openMap("blocks", new LogMVMapBuilder[Int, Array[Byte]])
    val signatures: MVMap[Int, BlockId] = database.openMap("signatures", new LogMVMapBuilder[Int, BlockId])
    val signaturesReverse: MVMap[BlockId, Int] = database.openMap("signaturesReverse", new LogMVMapBuilder[BlockId, Int])

    private val BlocksCacheSizeLimit: Int = 1000
    private val blocksCache = CacheBuilder.newBuilder()
      .maximumSize(BlocksCacheSizeLimit)
      .build[Integer, Block](
      new CacheLoader[Integer, Block]() {
        def load(height: Integer): Block = {
          val blockOpt = Try(Option(blocks.get(height))).toOption.flatten.flatMap(b => Block.parseBytes(b).recoverWith {
            case t: Throwable =>
              log.error("Block.parseBytes error", t)
              Failure(t)
          }.toOption)
          blockOpt.get
        }
      })
    val checkpoint: MVMap[Int, Checkpoint] = database.openMap("checkpoint", new LogMVMapBuilder[Int, Checkpoint])

    //TODO: remove when no blockchains without signaturesReverse remains
    if (signaturesReverse.size() != signatures.size()) {
      signaturesReverse.clear()
      signatures.keySet().asScala.foreach(k => signaturesReverse.put(signatures.get(k), k))
      database.commit()
    }

    val scoreMap: MVMap[Int, BigInt] = database.openMap("score", new LogMVMapBuilder[Int, BigInt])

    //if there are some uncommitted changes from last run, discard'em
    if (signatures.size() > 0) database.rollback()

    def writeBlock(height: Int, block: Block): Unit = {
      blocks.put(height, block.bytes)
      scoreMap.put(height, score() + block.blockScore)
      signatures.put(height, block.uniqueId)
      signaturesReverse.put(block.uniqueId, height)
    }

    def readBlock(height: Int): Option[Block] = {
      try {
        Some(blocksCache.get(height))
      } catch {
        case e: UncheckedExecutionException =>
          log.debug(s"There are no block at $height")
          None
      }
    }

    def deleteBlock(height: Int): Unit = {
      blocksCache.invalidate(height)
      blocks.remove(height)
      val vOpt = Option(signatures.remove(height))
      vOpt.map(v => signaturesReverse.remove(v))
    }

    def contains(id: BlockId): Boolean = Option(signaturesReverse.get(id)).isDefined

    def height(): Int = signatures.size()

    def heightOf(id: BlockId): Option[Int] = Option(signaturesReverse.get(id))

    def score(): BlockchainScore = if (height() > 0) scoreMap.get(height()) else 0

    def score(id: BlockId): BlockchainScore = heightOf(id).map(scoreMap.get(_)).getOrElse(0)
  }

  private val blockStorage: BlockchainPersistence = BlockchainPersistence(db)

  override def appendBlock(block: Block): Either[ValidationError, Unit] = synchronized {
    if ((height() == 0) || (lastBlock.uniqueId sameElements block.reference)) {
      val h = height() + 1
      blockStorage.writeBlock(h, block)
      Right(())
    } else {
      Left(CustomError(s"Appending block ${block.json} which parent is not last block in blockchain"))
    }
  }

  override def discardBlock(): History = synchronized {
    require(height() > 1, "Chain is empty or contains genesis block only, can't make rollback")
    val h = height()
    blockStorage.deleteBlock(h)
    this
  }

  override def blockAt(height: Int): Option[Block] = synchronized {
    blockStorage.readBlock(height)
  }

  override def lastBlockIds(howMany: Int): Seq[BlockId] =
    (Math.max(1, height() - howMany + 1) to height()).flatMap(i => Option(blockStorage.signatures.get(i)))
      .reverse

  override def contains(signature: Array[Byte]): Boolean = blockStorage.contains(signature)

  override def height(): Int = blockStorage.height()

  override def score(): BlockchainScore = blockStorage.score()

  override def scoreOf(id: BlockId): BlockchainScore = blockStorage.score(id)

  override def heightOf(blockSignature: Array[Byte]): Option[Int] = blockStorage.heightOf(blockSignature)

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

  override def getCheckpoint: Option[Checkpoint] = Option(blockStorage.checkpoint.get(0))

  override def setCheckpoint(c: Option[Checkpoint]): Unit = {
    if (c.isDefined) blockStorage.checkpoint.put(0, c.get)
    else blockStorage.checkpoint.remove(0)
  }
}
