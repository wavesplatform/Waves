package com.wavesplatform

import cats.kernel.Monoid
import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.transaction.Transaction.Type
import com.wavesplatform.transaction.ValidationError.{AliasDoesNotExist, GenericError}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.lease.{LeaseTransaction, LeaseTransactionV1}
import com.wavesplatform.utils.Paged
import play.api.libs.json._
import supertagged.TaggedType

import scala.reflect.ClassTag
import scala.util.Try

package object state {
  def safeSum(x: Long, y: Long): Long = Try(Math.addExact(x, y)).getOrElse(Long.MinValue)

  // common logic for addressTransactions method of BlockchainUpdaterImpl and CompositeBlockchain
  def addressTransactionsFromDiff(
      b: Blockchain,
      d: Option[Diff])(address: Address, types: Set[Type], count: Int, fromId: Option[ByteStr]): Either[String, Seq[(Int, Transaction)]] = {

    def transactionsFromDiff(d: Diff): Seq[(Int, Transaction, Set[Address])] = d.transactions.values.view.toSeq.reverse

    def withPagination(s: Seq[(Int, Transaction, Set[Address])]): Seq[(Int, Transaction, Set[Address])] =
      fromId match {
        case None     => s
        case Some(id) => s.dropWhile(_._2.id() != id).drop(1)
      }

    def withFilterAndLimit(txs: Seq[(Int, Transaction, Set[Address])]): Seq[(Int, Transaction)] =
      txs
        .collect {
          case (height, tx, addresses) if addresses(address) && (types.isEmpty || types.contains(tx.builder.typeId)) => (height, tx)
        }
        .take(count)

    def withRestFromBlockchain(s: Seq[(Int, Transaction)]): Either[String, Seq[(Int, Transaction)]] =
      s.length match {
        case `count`        => Right(s)
        case l if l < count => b.addressTransactions(address, types, count - l, None).map(s ++ _)
        case _              => Right(s.take(count))
      }

    def transactions: Diff => Either[String, Seq[(Int, Transaction)]] =
      withRestFromBlockchain _ compose withFilterAndLimit compose withPagination compose transactionsFromDiff

    d.fold(b.addressTransactions(address, types, count, fromId)) { diff =>
      fromId match {
        case Some(id) if !diff.transactions.contains(id) =>
          b.addressTransactions(address, types, count, fromId)
        case _ => transactions(diff)
      }
    }
  }

  implicit class EitherExt[L <: ValidationError, R](ei: Either[L, R]) {
    def liftValidationError[T <: Transaction](t: T): Either[ValidationError, R] = {
      ei.left.map(e => GenericError(e.toString))
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
    def resolveAlias(aoa: AddressOrAlias): Either[ValidationError, Address] =
      aoa match {
        case a: Address => Right(a)
        case a: Alias   => blockchain.resolveAlias(a)
      }

    def canCreateAlias(alias: Alias): Boolean = blockchain.resolveAlias(alias) match {
      case Left(AliasDoesNotExist(_)) => true
      case _                          => false
    }

    def effectiveBalance(address: Address, confirmations: Int, block: BlockId = blockchain.lastBlockId.getOrElse(ByteStr.empty)): Long = {
      val blockHeight = blockchain.heightOf(block).getOrElse(blockchain.height)
      val bottomLimit = (blockHeight - confirmations + 1).max(1).min(blockHeight)
      val balances    = blockchain.balanceSnapshots(address, bottomLimit, block)
      if (balances.isEmpty) 0L else balances.view.map(_.effectiveBalance).min
    }

    def balance(address: Address, atHeight: Int, confirmations: Int): Long = {
      val bottomLimit = (atHeight - confirmations + 1).max(1).min(atHeight)
      val block       = blockchain.blockAt(atHeight).getOrElse(throw new IllegalArgumentException(s"Invalid block height: $atHeight"))
      val balances    = blockchain.balanceSnapshots(address, bottomLimit, block.uniqueId)
      if (balances.isEmpty) 0L else balances.view.map(_.regularBalance).min
    }

    def aliasesOfAddress(address: Address): Seq[Alias] =
      blockchain
        .addressTransactions(address, Set(CreateAliasTransactionV1.typeId), Int.MaxValue, None)
        .explicitGet()
        .collect { case (_, a: CreateAliasTransaction) => a.alias }

    def activeLeases(address: Address): Seq[(Int, LeaseTransaction)] =
      blockchain
        .addressTransactions(address, Set(LeaseTransactionV1.typeId), Int.MaxValue, None)
        .explicitGet()
        .collect { case (h, l: LeaseTransaction) if blockchain.leaseDetails(l.id()).exists(_.isActive) => h -> l }

    def unsafeHeightOf(id: ByteStr): Int =
      blockchain
        .heightOf(id)
        .getOrElse(throw new IllegalStateException(s"Can't find a block: $id"))

    def wavesPortfolio(address: Address): Portfolio = Portfolio(
      blockchain.balance(address),
      blockchain.leaseBalance(address),
      Map.empty
    )
  }

  object AssetDistribution extends TaggedType[Map[Address, Long]]
  type AssetDistribution = AssetDistribution.Type

  implicit val dstMonoid: Monoid[AssetDistribution] = new Monoid[AssetDistribution] {
    override def empty: AssetDistribution = AssetDistribution(Map.empty[Address, Long])

    override def combine(x: AssetDistribution, y: AssetDistribution): AssetDistribution = {
      AssetDistribution(x ++ y)
    }
  }

  implicit val dstWrites: Writes[AssetDistribution] = Writes { dst =>
    Json
      .toJson(dst.map {
        case (addr, balance) => addr.stringRepr -> balance
      })
  }

  object AssetDistributionPage extends TaggedType[Paged[Address, AssetDistribution]]
  type AssetDistributionPage = AssetDistributionPage.Type

  implicit val dstPageWrites: Writes[AssetDistributionPage] = Writes { page =>
    JsObject(
      Map(
        "hasNext"  -> JsBoolean(page.hasNext),
        "lastItem" -> Json.toJson(page.lastItem.map(_.stringRepr)),
        "items"    -> Json.toJson(page.items)
      )
    )
  }

  object Height extends TaggedType[Int]
  type Height = Height.Type

  object TxNum extends TaggedType[Short]
  type TxNum = TxNum.Type

  object AddressId extends TaggedType[BigInt]
  type AddressId = AddressId.Type

  object TransactionId extends TaggedType[ByteStr]
  type TransactionId = TransactionId.Type
}
