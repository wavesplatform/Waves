package com.wavesplatform.state.extensions.impl

import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.state.extensions.AccountAggregations
import com.wavesplatform.state.{Blockchain, Diff, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction

private[state] final class CompositeAccountAggregations(blockchain: Blockchain, baseProvider: AccountAggregations, getDiff: () => Option[Diff])
    extends AccountAggregations {
  override def portfolio(a: Address): Portfolio = {
    val diffPf = {
      val full = getDiff().flatMap(_.portfolios.get(a)).getOrElse(Portfolio.empty)
      val nonNft = for {
        (IssuedAsset(id), balance) <- full.assets
        (_, tx: IssueTransaction)  <- blockchain.transactionInfo(id) if !tx.isNFT
      } yield (IssuedAsset(id), balance)
      full.copy(assets = nonNft)
    }

    Monoid.combine(baseProvider.portfolio(a), diffPf)
  }

  override def accountDataKeys(acc: Address): Set[String] = {
    val fromInner = baseProvider.accountDataKeys(acc)
    val fromDiff  = getDiff().flatMap(_.accountData.get(acc)).toSeq.flatMap(_.data.keys)
    (fromInner ++ fromDiff)
  }

}
