package com.wavesplatform.state.extensions.impl

import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.database.extensions.impl.LevelDBApiExtensions
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.extensions.ApiExtensions
import com.wavesplatform.state.{AssetDistribution, AssetDistributionPage, Blockchain, BlockchainUpdaterImpl, Diff, Height, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{Transaction, TransactionParser}
import monix.reactive.Observable

final class CompositeApiExtensions(blockchain: Blockchain, baseProvider: ApiExtensions, getDiff: () => Option[Diff]) extends ApiExtensions {
  override def addressTransactionsObservable(
      address: Address,
      types: Set[TransactionParser],
      fromId: Option[ByteStr]
  ): Observable[(Height, Transaction)] = {
    val fromDiff = for {
      diff                    <- getDiff().toIterable
      (height, tx, addresses) <- diff.transactions.values.toVector.reverse
    } yield (Height(height), tx, addresses)

    def withPagination(txs: Iterable[(Height, Transaction, Set[Address])]): Iterable[(Height, Transaction, Set[Address])] =
      fromId match {
        case None     => txs
        case Some(id) => txs.dropWhile(_._2.id() != id).drop(1)
      }

    def withFilterAndLimit(txs: Iterable[(Height, Transaction, Set[Address])]): Iterable[(Height, Transaction)] =
      txs
        .collect { case (height, tx, addresses) if addresses(address) && (types.isEmpty || types.contains(tx.builder)) => (height, tx) }

    Observable(
      Observable.fromIterable(withFilterAndLimit(withPagination(fromDiff)).map(tup => (tup._1, tup._2))),
      baseProvider.addressTransactionsObservable(address, types, fromId)
    ).concat
  }

  override def nftObservable(address: Address, from: Option[IssuedAsset]): Observable[IssueTransaction] = {
    val maybeDiff = getDiff()

    def nonZeroBalance(asset: IssuedAsset): Boolean = {
      val balanceFromDiff = for {
        diff      <- maybeDiff
        portfolio <- diff.portfolios.get(address)
        balance   <- portfolio.assets.get(asset)
      } yield balance

      balanceFromDiff.forall(_ > 0)
    }

    def nftFromDiff(diff: Diff, maybeAfter: Option[IssuedAsset]): Observable[IssueTransaction] = Observable.fromIterable {
      def assetStreamFromDiff(diff: Diff): Iterable[IssuedAsset] =
        diff.portfolios
          .get(address)
          .toIterable
          .flatMap(_.assets.keys)

      val assets = maybeAfter
        .fold(assetStreamFromDiff(diff)) { after =>
          assetStreamFromDiff(diff)
            .dropWhile(_ != after)
            .drop(1)
        }

      assets
        .filter(nonZeroBalance)
        .map {
          case IssuedAsset(assetId) =>
            val txFromDiff = diff.transactions.get(assetId).map(_._2)
            txFromDiff.orElse(blockchain.transactionInfo(assetId).map(_._2))
        }
        .collect { case itx: IssueTransaction if itx.isNFT => itx }
    }

    def nftFromBlockchain: Observable[IssueTransaction] =
      baseProvider
        .nftObservable(address, from)
        .filter(itx => nonZeroBalance(IssuedAsset(itx.assetId)))

    maybeDiff.fold(nftFromBlockchain) { diff =>
      from match {
        case None                                            => Observable(nftFromDiff(diff, from), nftFromBlockchain).concat
        case Some(asset) if diff.issuedAssets contains asset => Observable(nftFromDiff(diff, from), nftFromBlockchain).concat
        case _                                               => nftFromBlockchain
      }
    }
  }

  override def assetDistribution(assetId: IssuedAsset): AssetDistribution = {
    val fromInner = baseProvider.assetDistribution(assetId)
    val fromNg    = AssetDistribution(changedBalances(_.assets.getOrElse(assetId, 0L) != 0, blockchain.balance(_, assetId)))
    Monoid.combine(fromInner, fromNg)
  }

  override def assetDistributionAtHeight(
      assetId: IssuedAsset,
      height: Int,
      count: Int,
      fromAddress: Option[Address]
  ): Either[ValidationError, AssetDistributionPage] = {
    baseProvider.assetDistributionAtHeight(assetId, height, count, fromAddress)
  }

  override def wavesDistribution(height: Int): Either[ValidationError, Map[Address, Long]] = {
    getDiff().fold(baseProvider.wavesDistribution(height)) { _ =>
      val innerDistribution = baseProvider.wavesDistribution(height)
      if (height < blockchain.height) innerDistribution
      else {
        innerDistribution.map(_ ++ changedBalances(_.balance != 0, blockchain.balance(_)))
      }
    }
  }

  private def changedBalances(pred: Portfolio => Boolean, f: Address => Long): Map[Address, Long] = {
    getDiff()
      .fold(Map.empty[Address, Long]) { diff =>
        for {
          (address, p) <- diff.portfolios
          if pred(p)
        } yield address -> f(address)
      }
  }

  override def portfolio(a: Address): Portfolio = {
    val diffPf = {
      val full         = getDiff().flatMap(_.portfolios.get(a)).getOrElse(Portfolio.empty)
      val nonNftAssets = full.assets.filterNot { case (asset, _) => blockchain.isNFT(asset) }
      full.copy(assets = nonNftAssets)
    }

    Monoid.combine(baseProvider.portfolio(a), diffPf)
  }

  override def accountDataKeys(acc: Address): Set[String] = {
    val fromInner = baseProvider.accountDataKeys(acc)
    val fromDiff  = getDiff().flatMap(_.accountData.get(acc)).toSeq.flatMap(_.data.keys)
    (fromInner ++ fromDiff)
  }
}

object CompositeApiExtensions {
  def apply(bu: BlockchainUpdaterImpl): ApiExtensions = {
    new CompositeApiExtensions(bu, new LevelDBApiExtensions(bu.stableBlockchain.asInstanceOf[LevelDBWriter]), () => bu.maybeDiff)
  }
}
