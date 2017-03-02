package scorex.transaction

import scorex.account.{Account, AccountOrAlias, Alias, PublicKeyAccount}
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.state.storage.LeaseExtendedStateStorageI


object BalanceChangeCalculator {

  def balanceChanges(tx: Transaction): Seq[BalanceChange] = {
    tx match {
      case t: GenesisTransaction =>
        Seq(BalanceChange(AssetAcc(t.recipient, None), t.amount))

      case t: PaymentTransaction =>
        Seq(BalanceChange(AssetAcc(t.sender, None), -t.amount - t.fee), BalanceChange(AssetAcc(t.recipient, None), t.amount))

      case t: IssueTransaction =>
        Seq(BalanceChange(AssetAcc(t.sender, Some(t.assetId)), t.quantity), BalanceChange(AssetAcc(t.sender, t.assetFee._1), -t.assetFee._2))

      case t: TransferTransaction =>
        lazy val sameAssetForFee: Boolean = t.feeAssetId.map(fa => t.assetId.exists(_ sameElements fa)).getOrElse(t.assetId.isEmpty)
        val recipientCh = BalanceChange(AssetAcc(resolve(t.recipient), t.assetId), t.amount)
        val senderCh =
          if (sameAssetForFee) Seq(BalanceChange(AssetAcc(t.sender, t.assetId), -t.amount - t.fee))
          else Seq(BalanceChange(AssetAcc(t.sender, t.assetId), -t.amount), BalanceChange(AssetAcc(t.sender, t.feeAssetId), -t.fee))
        recipientCh +: senderCh

      case t: ReissueTransaction =>
        Seq(BalanceChange(AssetAcc(t.sender, Some(t.assetId)), t.quantity), BalanceChange(AssetAcc(t.sender, t.assetFee._1), -t.assetFee._2))

      case t: BurnTransaction =>
        Seq(BalanceChange(AssetAcc(t.sender, Some(t.assetId)), -t.amount), BalanceChange(AssetAcc(t.sender, t.assetFee._1), -t.assetFee._2))

      case t: ExchangeTransaction =>
        val matcherChange = Seq(BalanceChange(AssetAcc(t.buyOrder.matcherPublicKey, None), t.buyMatcherFee + t.sellMatcherFee - t.fee))
        val buyFeeChange = Seq(BalanceChange(AssetAcc(t.buyOrder.senderPublicKey, None), -t.buyMatcherFee))
        val sellFeeChange = Seq(BalanceChange(AssetAcc(t.sellOrder.senderPublicKey, None), -t.sellMatcherFee))

        val exchange: Seq[(PublicKeyAccount, (Option[AssetId], Long))] = Seq(
          (t.buyOrder.senderPublicKey, (t.buyOrder.spendAssetId, -t.buyOrder.getSpendAmount(t.price, t.amount).get)),
          (t.buyOrder.senderPublicKey, (t.buyOrder.receiveAssetId, t.buyOrder.getReceiveAmount(t.price, t.amount).get)),
          (t.sellOrder.senderPublicKey, (t.sellOrder.receiveAssetId, t.sellOrder.getReceiveAmount(t.price, t.amount).get)),
          (t.sellOrder.senderPublicKey, (t.sellOrder.spendAssetId, -t.sellOrder.getSpendAmount(t.price, t.amount).get))
        )

        buyFeeChange ++ sellFeeChange ++ matcherChange ++ exchange.map(c => BalanceChange(AssetAcc(c._1, c._2._1), c._2._2))

      case t: LeaseTransaction =>
        Seq(BalanceChange(AssetAcc(t.sender, None), -t.fee))

      case t: LeaseCancelTransaction =>
        Seq(BalanceChange(AssetAcc(t.sender, None), -t.fee))

      case _ => ???
    }
  }


  def effectiveBalanceChanges(storage: LeaseExtendedStateStorageI)(tx: Transaction): Seq[EffectiveBalanceChange] = tx match {
    case tx: LeaseTransaction =>
      Seq(EffectiveBalanceChange(tx.sender, -tx.amount - tx.fee),
        EffectiveBalanceChange(resolve(tx.recipient), tx.amount))
    case tx: LeaseCancelTransaction =>
      val leaseTx = storage.getExistedLeaseTx(tx.leaseId)
      Seq(
        EffectiveBalanceChange(tx.sender, leaseTx.amount - tx.fee),
        EffectiveBalanceChange(resolve(leaseTx.recipient), -leaseTx.amount))
    case _ => BalanceChangeCalculator.balanceChanges(tx).map(bc => {
      EffectiveBalanceChange(bc.assetAcc.account, bc.delta)
    })
  }

  def resolve(aoa: AccountOrAlias): Account = aoa match {
    case a: Account => a
    case a: Alias => ???
  }
}