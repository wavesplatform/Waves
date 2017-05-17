package com.wavesplatform.history

import org.h2.mvstore.MVStore
import scorex.account.Account
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.crypto.encode.Base58
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError.CustomError
import scorex.transaction.{HistoryWriter, ValidationError}
import scorex.utils.{LogMVMapBuilder, ScorexLogging}

class HistoryWriterImpl(db: MVStore) extends HistoryWriter with ScorexLogging {
  private val blockBodyByHeight = SynchronizedAcrossInstance(db.openMap("blocks", new LogMVMapBuilder[Int, Array[Byte]]))
  private val blockIdByHeight = SynchronizedAcrossInstance(db.openMap("signatures", new LogMVMapBuilder[Int, BlockId]))
  private val heightByBlockId = SynchronizedAcrossInstance(db.openMap("signaturesReverse", new LogMVMapBuilder[BlockId, Int]))
  private val scoreByHeight = SynchronizedAcrossInstance(db.openMap("score", new LogMVMapBuilder[Int, BigInt]))

  override def appendBlock(block: Block): Either[ValidationError, Unit] = synchronizeReadWrite { implicit lock =>
    if ((height() == 0) || (this.lastBlock.uniqueId sameElements block.reference)) {
      val h = height() + 1
      blockBodyByHeight.update(_.put(h, block.bytes))
      scoreByHeight.update(_.put(h, score() + block.blockScore))
      blockIdByHeight.update(_.put(h, block.uniqueId))
      heightByBlockId.update(_.put(block.uniqueId, h))
      db.commit()
      Right(())
    } else {
      Left(CustomError(s"Failed to append block ${block.encodedId} which parent(${Base58.encode(block.reference)} is not last block in blockchain"))
    }
  }

  override def discardBlock(): Unit = synchronizeReadWrite { implicit lock =>
    val h = height()
    blockBodyByHeight.update(_.remove(h))
    scoreByHeight.update(_.remove(h))
    val vOpt = Option(blockIdByHeight.update(_.remove(h)))
    vOpt.map(v => heightByBlockId.update(_.remove(v)))
    val _ = db.commit()
  }

  override def blockAt(height: Int): Option[Block] = synchronizeRead { implicit lock =>
    Option(blockBodyByHeight().get(height)).map(Block.parseBytes(_).get)
  }

  override def lastBlockIds(howMany: Int): Seq[BlockId] = synchronizeRead { implicit lock =>
    (Math.max(1, height() - howMany + 1) to height()).flatMap(i => Option(blockIdByHeight().get(i)))
      .reverse
  }

  override def height(): Int = synchronizeRead { implicit lock => blockIdByHeight().size() }

  override def score(): BlockchainScore = synchronizeRead { implicit lock =>
    if (height() > 0) scoreByHeight().get(height()) else 0
  }

  override def scoreOf(id: BlockId): BlockchainScore = synchronizeRead { implicit lock =>
    heightOf(id).map(scoreByHeight().get(_)).getOrElse(0)
  }

  override def heightOf(blockSignature: Array[Byte]): Option[Int] = synchronizeRead { implicit lock =>
    Option(heightByBlockId().get(blockSignature))
  }

  override def generatedBy(account: Account, from: Int, to: Int): Seq[Block] = synchronizeRead { implicit lock =>
    for {
      h <- from to to
      block <- blockAt(h)
      if block.signerData.generator.address.equals(account.address)
    } yield block
  }

  override def blockBytes(height: Int): Option[Array[Byte]] = synchronizeRead { implicit lock =>
    Option(blockBodyByHeight().get(height)) }
}

object HistoryWriterImpl {
  def apply(db: MVStore): Either[String, HistoryWriterImpl] = {
    val h = new HistoryWriterImpl(db)
    h.synchronizeRead { implicit lock =>
      if (Set(h.blockBodyByHeight().size(), h.blockIdByHeight().size(), h.heightByBlockId().size(), h.scoreByHeight().size()).size != 1)
        Left("Block storage is inconsistent")
      else
        Right(h)
    }
  }
}
