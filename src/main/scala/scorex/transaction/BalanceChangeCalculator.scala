package scorex.transaction

import scorex.account.{Account, AccountOrAlias, Alias, PublicKeyAccount}
import scorex.transaction.ValidationError.{AliasNotExists, UnsupportedTransactionType}
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.state.storage.LeaseExtendedStateStorageI


object BalanceChangeCalculator {

  def balanceChanges(state: State)(tx: Transaction): Either[ValidationError, Seq[BalanceChange]] = {
    tx match {
      case t: GenesisTransaction =>
        Right(Seq(BalanceChange(AssetAcc(t.recipient, None), t.amount)))

      case t: PaymentTransaction =>
        Right(Seq(BalanceChange(AssetAcc(t.sender, None), -t.amount - t.fee), BalanceChange(AssetAcc(t.recipient, None), t.amount)))

      case t: IssueTransaction =>
        Right(Seq(BalanceChange(AssetAcc(t.sender, Some(t.assetId)), t.quantity), BalanceChange(AssetAcc(t.sender, t.assetFee._1), -t.assetFee._2)))

      case t: TransferTransaction => resolveAddressOrAlias(state: State)(t, t.recipient).map(recipient => {
        lazy val sameAssetForFee: Boolean = t.feeAssetId.map(fa => t.assetId.exists(_ sameElements fa)).getOrElse(t.assetId.isEmpty)
        val recipientCh = BalanceChange(AssetAcc(recipient, t.assetId), t.amount)
        val senderCh =
          if (sameAssetForFee) Seq(BalanceChange(AssetAcc(t.sender, t.assetId), -t.amount - t.fee))
          else Seq(BalanceChange(AssetAcc(t.sender, t.assetId), -t.amount), BalanceChange(AssetAcc(t.sender, t.feeAssetId), -t.fee))
        recipientCh +: senderCh
      })

      case t: ReissueTransaction =>
        Right(Seq(BalanceChange(AssetAcc(t.sender, Some(t.assetId)), t.quantity), BalanceChange(AssetAcc(t.sender, t.assetFee._1), -t.assetFee._2)))

      case t: BurnTransaction =>
        Right(Seq(BalanceChange(AssetAcc(t.sender, Some(t.assetId)), -t.amount), BalanceChange(AssetAcc(t.sender, t.assetFee._1), -t.assetFee._2)))

      case t: ExchangeTransaction =>
        val matcherChange = Seq(BalanceChange(AssetAcc(t.buyOrder.matcherPublicKey, None), t.buyMatcherFee + t.sellMatcherFee - t.fee))
        val buyFeeChange = Seq(BalanceChange(AssetAcc(t.buyOrder.senderPublicKey, None), -t.buyMatcherFee))
        val sellFeeChange = Seq(BalanceChange(AssetAcc(t.sellOrder.senderPublicKey, None), -t.sellMatcherFee))

        val exchange = Seq(
          (t.buyOrder.senderPublicKey, (t.buyOrder.assetPair.priceAsset, -t.buyOrder.getSpendAmount(t.price, t.amount).right.get)),
          (t.buyOrder.senderPublicKey, (t.buyOrder.assetPair.amountAsset, t.buyOrder.getReceiveAmount(t.price, t.amount).right.get)),
          (t.sellOrder.senderPublicKey, (t.sellOrder.assetPair.priceAsset, t.sellOrder.getReceiveAmount(t.price, t.amount).right.get)),
          (t.sellOrder.senderPublicKey, (t.sellOrder.assetPair.amountAsset, -t.sellOrder.getSpendAmount(t.price, t.amount).right.get))
        )

        Right(buyFeeChange ++ sellFeeChange ++ matcherChange ++
          exchange.map(c => BalanceChange(AssetAcc(c._1, c._2._1), c._2._2)))

      case t: LeaseTransaction =>
        Right(Seq(BalanceChange(AssetAcc(t.sender, None), -t.fee)))

      case t: LeaseCancelTransaction =>
        Right(Seq(BalanceChange(AssetAcc(t.sender, None), -t.fee)))

      case t: CreateAliasTransaction =>
        Right(Seq(BalanceChange(AssetAcc(t.sender, None), -t.fee)))

      case t => Left(UnsupportedTransactionType(t))
    }
  }

  def effectiveBalanceChanges(state: State)(tx: Transaction): Either[ValidationError, Seq[EffectiveBalanceChange]] = tx match {
    case tx: LeaseTransaction => resolveAddressOrAlias(state)(tx, tx.recipient).map(recipient => {
      Seq(EffectiveBalanceChange(tx.sender, -tx.amount - tx.fee),
        EffectiveBalanceChange(recipient, tx.amount))
    })
    case tx: LeaseCancelTransaction =>
      val leaseTx = state.findTransaction[LeaseTransaction](tx.leaseId).get
      resolveAddressOrAlias(state)(tx, leaseTx.recipient).map(recipient => {
        Seq(
          EffectiveBalanceChange(tx.sender, leaseTx.amount - tx.fee),
          EffectiveBalanceChange(recipient, -leaseTx.amount))
      })

    case _ => BalanceChangeCalculator.balanceChanges(state)(tx).map(_.flatMap(bc => {
      bc.assetAcc.assetId match {
        case Some(_) => None
        case None => Some(EffectiveBalanceChange(bc.assetAcc.account, bc.delta))
      }
    }))
  }

  def resolveAddressOrAlias(state: State)(tx: Transaction, aoa: AccountOrAlias): Either[ValidationError, Account] = aoa match {
    case a: Account => Right(a)
    case al: Alias => state.resolveAlias(al) match {
      case Some(ac) => Right(ac)
      case None => Left(AliasNotExists(al))
    }
  }
}
