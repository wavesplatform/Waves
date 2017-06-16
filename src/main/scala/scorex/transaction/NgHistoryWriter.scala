package scorex.transaction

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.state2._
import scorex.block.Block.BlockId
import scorex.block.{Block, MicroBlock}
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError.GenericError

trait NgHistoryWriter extends HistoryWriter {
  def appendMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit]
}

class NgHistoryWriterImpl(inner: HistoryWriter) extends NgHistoryWriter {

  private val baseBlock = Synchronized(Option.empty[Block])
  private val micros = Synchronized(List.empty[MicroBlock])

  private def totalBlock(): Option[Block] = write { implicit l =>
    baseBlock().map(base => {
      val ms = micros()
      if (ms.isEmpty) {
        base
      } else {
        base.copy(
          signerData = base.signerData.copy(signature = ms.head.totalResBlockSig),
          transactionData = base.transactionData ++ ms.map(_.transactions).reverse.flatten)
      }
    })
  }

  override def synchronizationToken: ReentrantReadWriteLock = inner.synchronizationToken

  override def appendBlock(block: Block): Either[ValidationError, Unit] = write { implicit l =>
    if (inner.height() == 0 || inner.lastBlock.uniqueId == block.reference) {
      micros.set(List.empty)
      baseBlock.set(Some(block))
      Right(())
    } else if (containsLocalBlock(block.reference)) {
      inner.appendBlock(forgeBlock(block.reference).get).explicitGet()
      micros.set(List.empty)
      baseBlock.set(Some(block))
      Right(())
    } else {
      Left(GenericError(s"Failed to append block ${block.encodedId} because its parent(${block.reference.base58} " +
        s"is neither last one in persisted blockchain nor liquid"))
    }
  }

  override def discardBlock(): Unit = write { implicit l =>
    if (baseBlock().isDefined) {
      baseBlock.set(None)
      micros.set(List.empty)
    } else {
      inner.discardBlock()
    }
  }

  override def height(): Int = read { implicit l =>
    inner.height() + baseBlock().map(_ => 1).getOrElse(0)
  }

  override def blockBytes(height: Int): Option[Array[Byte]] = read { implicit l =>
    inner.blockBytes(height).orElse(if (height == inner.height() + 1) totalBlock().map(_.bytes) else None)
  }

  override def scoreOf(blockId: BlockId): Option[BlockchainScore] = read { implicit l =>
    inner.scoreOf(blockId)
      .orElse(if (containsLocalBlock(blockId))
        Some(inner.score() + baseBlock().get.blockScore)
      else None)
  }

  override def heightOf(blockId: BlockId): Option[Int] = read { implicit l =>
    lazy val innerHeight = inner.height()
    inner.heightOf(blockId).orElse(if (containsLocalBlock(blockId))
      Some(innerHeight + 1)
    else
      None)
  }

  override def lastBlockIds(howMany: Int): Seq[BlockId] = read { implicit l =>
    if (baseBlock().isDefined)
      totalBlock().get.uniqueId +: inner.lastBlockIds(howMany - 1)
    else
      inner.lastBlockIds(howMany)
  }

  override def appendMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit] = write { implicit l =>
    baseBlock() match {
      case Some(base) =>
        micros().headOption match {
          case None =>
            if (base.uniqueId == microBlock.prevResBlockSig) {
              micros.set(microBlock +: micros())
              Right(())
            }
            else {
              Left(GenericError(s"MicroBlock $microBlock can't be appended because it is first micro and it doesn't reference existing base block"))
            }
          case Some(prevMicro) =>
            if (prevMicro.totalResBlockSig == microBlock.prevResBlockSig) {
              micros.set(microBlock +: micros())
              Right(())
            } else {
              Left(GenericError(s"Microblock $microBlock can't be appended because it doesn't reference last known microBlock"))
            }
        }
      case None =>
        Left(GenericError(s"MicroBlock $microBlock can't be appended because no liquid block exists"))
    }
  }

  private def containsLocalBlock(blockId: BlockId): Boolean = read { implicit l =>
    baseBlock().find(_.uniqueId == blockId)
      .orElse(micros().find(_.totalResBlockSig == blockId)).isDefined
  }

  private def forgeBlock(id: BlockId): Option[Block] = ???
}

object NgHistoryWriter {
  //  override def addMicro(microBlock: MicroBlock, microDiff: Diff): Either[ValidationError, Unit] = write { implicit l =>
  //    val currentMicros = micros()
  //    if (currentMicros.exists(_.totalResBlockSig == microBlock.totalResBlockSig)) Right(())
  //    else {
  //      lazy val isFirstMicro = baseBlock().exists(_.uniqueId == microBlock.prevResBlockSig)
  //      lazy val isNextMicro = currentMicros.headOption.exists(_.totalResBlockSig == microBlock.prevResBlockSig)
  //      if (isFirstMicro || isNextMicro) {
  //        micros.set(microBlock +: currentMicros)
  //        totalDiff.set(Monoid.combine(totalDiff(), microDiff))
  //        Right(())
  //      }
  //      else Left(GenericError("Referenced block/microBlock not found or has been referenced, or the block has been persisted"))
  //    }
  //  }

}