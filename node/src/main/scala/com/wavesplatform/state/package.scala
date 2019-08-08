package com.wavesplatform

import cats.kernel.Monoid
import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.GeneratingBalanceProvider
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.extensions.{AddressTransactions, Distributions}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.{AliasDoesNotExist, GenericError}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.utils.Paged
import monix.reactive.Observable
import play.api.libs.json._
import supertagged.TaggedType

import scala.concurrent.duration.Duration
import scala.reflect.ClassTag
import scala.util.Try

package object state {
  def safeSum(x: Long, y: Long): Long = Try(Math.addExact(x, y)).getOrElse(Long.MinValue)

  private[state] def nftListFromDiff(blockchain: Blockchain, distr: Distributions, maybeDiff: Option[Diff])(
      address: Address,
      maybeAfter: Option[IssuedAsset]): Observable[IssueTransaction] = {

    def nonZeroBalance(asset: IssuedAsset): Boolean = {
      val balanceFromDiff = for {
        diff      <- maybeDiff
        portfolio <- diff.portfolios.get(address)
        balance   <- portfolio.assets.get(asset)
      } yield balance

      !balanceFromDiff.exists(_ < 0)
    }
    def transactionFromDiff(diff: Diff, id: ByteStr): Option[Transaction] = {
      diff.transactions.get(id).map(_._2)
    }

    def assetStreamFromDiff(diff: Diff): Iterable[IssuedAsset] = {
      diff.portfolios
        .get(address)
        .toIterable
        .flatMap(_.assets.keys)
    }

    def nftFromDiff(diff: Diff, maybeAfter: Option[IssuedAsset]): Observable[IssueTransaction] = Observable.fromIterable {
      maybeAfter
        .fold(assetStreamFromDiff(diff)) { after =>
          assetStreamFromDiff(diff)
            .dropWhile(_ != after)
            .drop(1)
        }
        .filter(nonZeroBalance)
        .map { asset =>
          transactionFromDiff(diff, asset.id)
            .orElse(blockchain.transactionInfo(asset.id).map(_._2))
        }
        .collect {
          case Some(itx: IssueTransaction) if itx.isNFT => itx
        }
    }

    def nftFromBlockchain: Observable[IssueTransaction] =
      distr
        .nftObservable(address, maybeAfter)
        .filter { itx =>
          val asset = IssuedAsset(itx.assetId)
          nonZeroBalance(asset)
        }

    maybeDiff.fold(nftFromBlockchain) { diff =>
      maybeAfter match {
        case None                                         => Observable(nftFromDiff(diff, maybeAfter), nftFromBlockchain).concat
        case Some(asset) if diff.issuedAssets contains asset => Observable(nftFromDiff(diff, maybeAfter), nftFromBlockchain).concat
        case _                                            => nftFromBlockchain
      }
    }
  }

  // common logic for addressTransactions method of BlockchainUpdaterImpl and CompositeBlockchain
  def addressTransactionsCompose(at: AddressTransactions, fromDiffIter: Observable[(Height, Transaction, Set[Address])])(
      address: Address,
      types: Set[TransactionParser],
      fromId: Option[ByteStr]): Observable[(Height, Transaction)] = {

    def withPagination(txs: Observable[(Height, Transaction, Set[Address])]): Observable[(Height, Transaction, Set[Address])] =
      fromId match {
        case None     => txs
        case Some(id) => txs.dropWhile(_._2.id() != id).drop(1)
      }

    def withFilterAndLimit(txs: Observable[(Height, Transaction, Set[Address])]): Observable[(Height, Transaction)] =
      txs
        .collect { case (height, tx, addresses) if addresses(address) && (types.isEmpty || types.contains(tx.builder)) => (height, tx) }

    Observable(
      withFilterAndLimit(withPagination(fromDiffIter)).map(tup => (tup._1, tup._2)),
      at.addressTransactionsObservable(address, types, fromId)
    ).concat
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
    def blockAt(height: Int): Option[Block]        = blockchain.blockBytes(height).flatMap(bb => Block.parseBytes(bb).toOption)

    def lastBlockId: Option[ByteStr]     = blockchain.lastBlock.map(_.uniqueId)
    def lastBlockTimestamp: Option[Long] = blockchain.lastBlock.map(_.timestamp)

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
      val (block, _)  = blockchain.blockHeaderAndSize(atHeight).getOrElse(throw new IllegalArgumentException(s"Invalid block height: $atHeight"))
      val balances    = blockchain.balanceSnapshots(address, bottomLimit, block.uniqueId)
      if (balances.isEmpty) 0L else balances.view.map(_.regularBalance).min
    }

    def aliasesOfAddress(address: Address): Seq[Alias] = {
      import monix.execution.Scheduler.Implicits.global

      blockchain
        .addressTransactionsObservable(address, Set(CreateAliasTransactionV1, CreateAliasTransactionV2), None)
        .collect {
          case (_, a: CreateAliasTransaction) => a.alias
        }
        .toListL
        .runSyncUnsafe(Duration.Inf)
    }

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

  object Height extends TaggedType[Int]
  type Height = Height.Type

  object TxNum extends TaggedType[Short]
  type TxNum = TxNum.Type

  object AddressId extends TaggedType[BigInt]
  type AddressId = AddressId.Type

  object TransactionId extends TaggedType[ByteStr]
  type TransactionId = TransactionId.Type
}
