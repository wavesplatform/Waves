package com.wavesplatform

import scorex.block.Block
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{AssetId, Transaction, ValidationError}

import scala.reflect.ClassTag
import scala.util.{Left, Right, Try}

package object state {
  def safeSum(x: Long, y: Long): Long = Try(Math.addExact(x, y)).getOrElse(Long.MinValue)

  implicit class EitherExt[L <: ValidationError, R](ei: Either[L, R]) {
    def liftValidationError[T <: Transaction](t: T): Either[ValidationError, R] = {
      ei.left.map(e => GenericError(e.toString))
    }
  }

  implicit class EitherExt2[A, B](ei: Either[A, B]) {
    def explicitGet(): B = ei match {
      case Left(value)  => throw new Exception(value.toString)
      case Right(value) => value
    }
  }

  implicit class Cast[A](a: A) {
    def cast[B: ClassTag]: Option[B] = {
      a match {
        case b: B => Some(b)
        case _    => None
      }
    }
  }

  implicit class BlockchainExt(history: Blockchain) {
    def isEmpty: Boolean = history.height == 0

    def contains(block: Block): Boolean       = history.contains(block.uniqueId)
    def contains(signature: ByteStr): Boolean = history.heightOf(signature).isDefined

    def blockById(blockId: ByteStr): Option[Block] = history.blockBytes(blockId).flatMap(bb => Block.parseBytes(bb).toOption)
    def blockAt(height: Int): Option[Block]        = history.blockBytes(height).flatMap(bb => Block.parseBytes(bb).toOption)

    def lastBlockHeaderAndSize: Option[(Block, Int)] = history.lastBlock.map(b => (b, b.bytes().length))
    def lastBlockId: Option[AssetId]                 = history.lastBlockHeaderAndSize.map(_._1.signerData.signature)
    def lastBlockTimestamp: Option[Long]             = history.lastBlockHeaderAndSize.map(_._1.timestamp)

    def lastBlocks(howMany: Int): Seq[Block] = {
      (Math.max(1, history.height - howMany + 1) to history.height).flatMap(history.blockAt).reverse
    }

    def genesis: Block = history.blockAt(1).get
  }

}
