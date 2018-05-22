package com.wavesplatform

import scorex.account.{Address, AddressOrAlias, Alias}
import scorex.block.Block
import scorex.transaction.ValidationError.{AliasDoesNotExist, GenericError}
import scorex.transaction.lease.{LeaseTransaction, LeaseTransactionV1}
import scorex.transaction._

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

  implicit class BlockchainExt(blockchain: Blockchain) {
    def isEmpty: Boolean = blockchain.height == 0

    def contains(block: Block): Boolean       = blockchain.contains(block.uniqueId)
    def contains(signature: ByteStr): Boolean = blockchain.heightOf(signature).isDefined

    def blockById(blockId: ByteStr): Option[Block] = blockchain.blockBytes(blockId).flatMap(bb => Block.parseBytes(bb).toOption)
    def blockAt(height: Int): Option[Block]        = blockchain.blockBytes(height).flatMap(bb => Block.parseBytes(bb).toOption)

    def lastBlockHeaderAndSize: Option[(Block, Int)] = blockchain.lastBlock.map(b => (b, b.bytes().length))
    def lastBlockId: Option[ByteStr]                 = blockchain.lastBlockHeaderAndSize.map(_._1.uniqueId)
    def lastBlockTimestamp: Option[Long]             = blockchain.lastBlockHeaderAndSize.map(_._1.timestamp)

    def lastBlocks(howMany: Int): Seq[Block] = {
      (Math.max(1, blockchain.height - howMany + 1) to blockchain.height).flatMap(blockchain.blockAt).reverse
    }

    def genesis: Block = blockchain.blockAt(1).get
    def resolveAliasEi[T <: Transaction](aoa: AddressOrAlias): Either[ValidationError, Address] =
      aoa match {
        case a: Address => Right(a)
        case a: Alias   => blockchain.resolveAlias(a).toRight(AliasDoesNotExist(a))
      }

    def effectiveBalance(address: Address, atHeight: Int, confirmations: Int): Long = {
      val bottomLimit = (atHeight - confirmations + 1).max(1).min(atHeight)
      val balances    = blockchain.balanceSnapshots(address, bottomLimit, atHeight)
      if (balances.isEmpty) 0L else balances.view.map(_.effectiveBalance).min
    }

    def balance(address: Address, atHeight: Int, confirmations: Int): Long = {
      val bottomLimit = (atHeight - confirmations + 1).max(1).min(atHeight)
      val balances    = blockchain.balanceSnapshots(address, bottomLimit, atHeight)
      if (balances.isEmpty) 0L else balances.view.map(_.regularBalance).min
    }

    def aliasesOfAddress(address: Address): Seq[Alias] =
      blockchain
        .addressTransactions(address, Set(CreateAliasTransactionV1.typeId), Int.MaxValue, 0)
        .collect { case (_, a: CreateAliasTransaction) => a.alias }

    def activeLeases(address: Address): Seq[(Int, LeaseTransaction)] =
      blockchain
        .addressTransactions(address, Set(LeaseTransactionV1.typeId), Int.MaxValue, 0)
        .collect { case (h, l: LeaseTransaction) if blockchain.leaseDetails(l.id()).exists(_.isActive) => h -> l }

    def unsafeHeightOf(id: ByteStr): Int =
      blockchain
        .heightOf(id)
        .getOrElse(throw new IllegalStateException(s"Can't find a block: $id"))
  }

}
