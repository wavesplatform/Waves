package com.wavesplatform

import cats.kernel.Monoid
import com.google.common.primitives.Ints
import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.GeneratingBalanceProvider
import com.wavesplatform.database.{readIntSeq, writeIntSeq}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.{AliasDoesNotExist, GenericError}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.utils.{CloseableIterator, Paged}
import play.api.libs.json._
import supertagged.TaggedType

import scala.reflect.ClassTag
import scala.util.Try

package object state {
  def safeSum(x: Long, y: Long): Long = Try(Math.addExact(x, y)).getOrElse(Long.MinValue)

  def nftListFromDiff(b: Blockchain, d: Option[Diff])(address: Address, after: Option[IssuedAsset]): CloseableIterator[IssueTransaction] = {
    def transactionFromDiff(d: Diff, id: ByteStr): Option[Transaction] = {
      d.transactions.get(id).map(_._2)
    }

    def assetStreamFromDiff(d: Diff): Iterator[IssuedAsset] = {
      d.portfolios
        .get(address)
        .toIterator
        .flatMap(_.assets.keysIterator)
    }

    def nftFromDiff(diff: Diff, maybeAfter: Option[IssuedAsset]): Iterator[IssueTransaction] = {
      after
        .fold(assetStreamFromDiff(diff)) { after =>
          assetStreamFromDiff(diff)
            .dropWhile(_ != after)
            .drop(1)
        }
        .map { asset =>
          transactionFromDiff(diff, asset.id)
            .orElse(b.transactionInfo(asset.id).map(_._2))
        }
        .collect {
          case itx: IssueTransaction if itx.isNFT => itx
        } ++ b.nftList(address, None)
    }

    d.fold(b.nftList(address, after)) { d =>
      after match {
        case None                                         => nftFromDiff(d, after) ++ b.nftList(address, after)
        case Some(asset) if d.issuedAssets contains asset => nftFromDiff(d, after) ++ b.nftList(address, None)
        case _                                            => b.nftList(address, after)
      }
    }
  }

  // common logic for addressTransactions method of BlockchainUpdaterImpl and CompositeBlockchain
  def addressTransactionsFromDiff(b: Blockchain, d: Option[Diff])(address: Address,
                                                                  types: Set[TransactionParser],
                                                                  fromId: Option[ByteStr]): CloseableIterator[(Height, Transaction)] = {

    def transactionsFromDiff(d: Diff): Iterator[(Int, Transaction, Set[Address])] =
      d.transactions.values.toSeq.reverseIterator

    def withPagination(txs: Iterator[(Int, Transaction, Set[Address])]): Iterator[(Int, Transaction, Set[Address])] =
      fromId match {
        case None     => txs
        case Some(id) => txs.dropWhile(_._2.id() != id).drop(1)
      }

    def withFilterAndLimit(txs: Iterator[(Int, Transaction, Set[Address])]): Iterator[(Int, Transaction)] =
      txs
        .collect { case (height, tx, addresses) if addresses(address) && (types.isEmpty || types.contains(tx.builder)) => (height, tx) }

    def transactions: Diff => Iterator[(Int, Transaction)] =
      withFilterAndLimit _ compose withPagination compose transactionsFromDiff

    d.fold(b.addressTransactions(address, types, fromId)) { diff =>
      fromId match {
        case Some(id) if !diff.transactions.contains(id) => b.addressTransactions(address, types, fromId)
        case _ =>
          val diffTxs = transactions(diff).map(kv => (Height(kv._1), kv._2))
          CloseableIterator.seq(diffTxs, b.addressTransactions(address, types, None))
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

  implicit class BlockchainExt(private val blockchain: Blockchain) extends AnyVal {
    def isEmpty: Boolean = blockchain.height == 0

    def contains(block: Block): Boolean       = blockchain.contains(block.uniqueId)
    def contains(signature: ByteStr): Boolean = blockchain.heightOf(signature).isDefined

    def blockById(blockId: ByteStr): Option[Block] = blockchain.blockBytes(blockId).flatMap(bb => Block.parseBytes(bb).toOption)

    def blockAt(height: Height): Option[Block] = blockchain.blockBytes(height).flatMap(bb => Block.parseBytes(bb).toOption)

    def lastBlockId: Option[ByteStr]     = blockchain.lastBlock.map(_.uniqueId)
    def lastBlockTimestamp: Option[Long] = blockchain.lastBlock.map(_.timestamp)

    def lastBlocks(howMany: Int): Seq[Block] = {
      (Math.max(1, blockchain.height - howMany + 1) to blockchain.height).flatMap(h => blockchain.blockAt(Height @@ h)).reverse
    }

    def genesis: Block = blockchain.blockAt(Height.Genesis).get
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

    def balance(address: Address, atHeight: Height, confirmations: Int): Long = {
      val bottomLimit = (atHeight - confirmations + 1).max(1).min(atHeight)
      val block       = blockchain.blockAt(atHeight).getOrElse(throw new IllegalArgumentException(s"Invalid block height: $atHeight"))
      val balances    = blockchain.balanceSnapshots(address, bottomLimit, block.uniqueId)
      if (balances.isEmpty) 0L else balances.view.map(_.regularBalance).min
    }

    def aliasesOfAddress(address: Address): CloseableIterator[Alias] =
      blockchain
        .addressTransactions(address, TransactionParsers.forTypes(CreateAliasTransaction.typeId), None)
        .collect { case (_, a: CreateAliasTransaction) => a.alias }

    def activeLeases(address: Address): CloseableIterator[(Int, LeaseTransaction)] =
      blockchain
        .addressTransactions(address, TransactionParsers.forTypes(LeaseTransaction.typeId), None)
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

    def isMiningAllowed(height: Int, effectiveBalance: Long): Boolean =
      GeneratingBalanceProvider.isMiningAllowed(blockchain, height, effectiveBalance)

    def isEffectiveBalanceValid(height: Int, block: Block, effectiveBalance: Long): Boolean =
      GeneratingBalanceProvider.isEffectiveBalanceValid(blockchain, height, block, effectiveBalance)

    def generatingBalance(account: Address, blockId: BlockId = ByteStr.empty): Long =
      GeneratingBalanceProvider.balance(blockchain, account, blockId)
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

  object Height extends TaggedType[Int] {
    val Genesis: Height = apply(1)
  }
  type Height = Height.Type

  object TxNum extends TaggedType[Short]
  type TxNum = TxNum.Type

  object AddressId extends TaggedType[Long] {
    def toBytes(addrId: Long) = Ints.toByteArray(addrId.toInt)
    def fromBytes(bs: Array[Byte]) = apply(Integer.toUnsignedLong(Ints.fromByteArray(bs)))

    def readSeq = readIntSeq _ andThen (_.map(Integer.toUnsignedLong).map(AddressId @@ _))

    def writeSeq = ((_: Seq[AddressId]).map(_.toInt)) andThen writeIntSeq
  }
  type AddressId = AddressId.Type

  object TransactionId extends TaggedType[ByteStr]
  type TransactionId = TransactionId.Type
}
