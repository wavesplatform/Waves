package com.wavesplatform.state.extensions.composite

import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.extensions.Distributions
import com.wavesplatform.state.{AssetDistribution, AssetDistributionPage, Blockchain, Diff, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import monix.reactive.Observable

private[state] final class CompositeDistributions(blockchain: Blockchain, baseProvider: Distributions, getDiff: () => Option[Diff])
    extends Distributions {
  override def portfolio(a: Address): Portfolio = {
    val diffPf = {
      val full = getDiff().flatMap(_.portfolios.get(a)).getOrElse(Portfolio.empty)
      val nonNft = for {
        (IssuedAsset(id), balance) <- full.assets
        (_, tx: IssueTransaction)  <- blockchain.transactionInfo(id) if !tx.isNFT(blockchain)
      } yield (IssuedAsset(id), balance)
      full.copy(assets = nonNft)
    }

    Monoid.combine(baseProvider.portfolio(a), diffPf)
  }

  override def nftObservable(address: Address, from: Option[IssuedAsset]): Observable[IssueTransaction] =
    com.wavesplatform.state.nftListFromDiff(blockchain, baseProvider, getDiff())(address, from)

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
}
