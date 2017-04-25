package com.wavesplatform.history

import com.google.common.cache.{CacheBuilder, CacheLoader}
import scorex.account.Account
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.crypto.encode.Base58
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError.CustomError
import scorex.transaction.{History, HistoryWriter, ValidationError}
import scorex.utils.ScorexLogging

class HistoryWriterImpl(storage: HistoryStorage) extends History with HistoryWriter with ScorexLogging {

  private val BlocksCacheSizeLimit: Int = 1000
  private val blocksCache = CacheBuilder.newBuilder()
    .maximumSize(BlocksCacheSizeLimit)
    .build(CacheLoader.from[Integer, Block] { height =>
      Block.parseBytes(storage.blockBodyByHeight.get(height)).get
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
      Left(CustomError(s"Failed to append block ${block.encodedId} which parent(${Base58.encode(block.reference)} is not last block in blockchain"))
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

  override def blockAt(height: Int): Option[Block] = scala.util.control.Exception.allCatch.opt(blocksCache.get(height))

  override def lastBlockIds(howMany: Int): Seq[BlockId] =
    (Math.max(1, height() - howMany + 1) to height()).flatMap(i => Option(storage.blockIdByHeight.get(i)))
      .reverse

  override def height(): Int = storage.blockIdByHeight.size()

  override def score(): BlockchainScore = if (height() > 0) storage.scoreByHeight.get(height()) else 0

  override def scoreOf(id: BlockId): BlockchainScore = heightOf(id).map(storage.scoreByHeight.get(_)).getOrElse(0)

  override def heightOf(blockSignature: Array[Byte]): Option[Int] = Option(storage.heightByBlockId.get(blockSignature))

  override def generatedBy(account: Account, from: Int, to: Int): Seq[Block] =
    for {
      h <- from to to
      block <- blockAt(h)
      if block.signerData.generator.address.equals(account.address)
    } yield block
}
