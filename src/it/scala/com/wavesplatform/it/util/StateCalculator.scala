package com.wavesplatform.it.util

import com.wavesplatform.it.util.StateCalculator.{Balances, Result}
import com.wavesplatform.state2.ByteStr
import scorex.account.{Account, AccountOrAlias, Alias}
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.{AssetId, CreateAliasTransaction, PaymentTransaction, Transaction}


object StateCalculator {

  object Balances {
    val zero = Balances(0, 0)
  }

  case class Balances(normal: Long, effective: Long)

  case class Result(wavesBalances: Map[Account, Balances] = Map.empty.withDefaultValue(Balances.zero),
                    assetsBalances: Map[Account, Map[AssetId, Long]] = Map.empty.withDefaultValue(Map.empty.withDefaultValue(0L)),
                    issuedAssets: Map[AssetId, Long] = Map.empty.withDefaultValue(0L),
                    aliases: Map[String, Account] = Map.empty,
                    leases: Map[ByteStr, LeaseTransaction] = Map.empty
                   )

}

class StateCalculator {
  def calculate(startBalances: Map[Account, Long], txs: Seq[Transaction]): Result = {
    txs.foldLeft(Result()) {
      case (r@Result(wavesBalances, assetsBalances, issuedAssets, aliases, leases), tx) =>
        def accountOrAliasToAccount(accountOrAlias: AccountOrAlias): Account = {
          accountOrAlias match {
            case a: Account => a
            case al: Alias => aliases(al.stringRepr)
          }
        }

        tx match {
          case PaymentTransaction(sender, recipient, amount, fee, _, _) =>
            val senderBalance = wavesBalances(sender.toAccount)
            val recipientBalance = wavesBalances(recipient)
            r.copy(
              wavesBalances.updated(recipient, Balances(recipientBalance.normal + amount, recipientBalance.effective + amount))
                .updated(sender.toAccount, Balances(senderBalance.normal - amount - fee, senderBalance.effective - amount - fee)))
          case i@IssueTransaction(sender, _, _, quantity, _, _, fee, _, _) =>
            val senderAccount = sender.toAccount
            val senderBalance = wavesBalances(senderAccount)
            r.copy(wavesBalances = wavesBalances.updated(senderAccount, Balances(senderBalance.normal - fee, senderBalance.effective - fee)))
              .copy(assetsBalances = assetsBalances.updated(senderAccount, assetsBalances(senderAccount) + (i.id -> quantity)))
              .copy(issuedAssets = issuedAssets.updated(i.id, quantity))
          case ReissueTransaction(sender, assetId, quantity, _, fee, _, _) =>
            val senderAccount = sender.toAccount
            val senderBalance = wavesBalances(senderAccount)
            val prevAssetBalances = assetsBalances(senderAccount)

            r.copy(wavesBalances = wavesBalances.updated(senderAccount, Balances(senderBalance.normal - fee, senderBalance.effective - fee)),
              assetsBalances =
                assetsBalances.updated(senderAccount, assetsBalances(senderAccount)
                  .updated(assetId, prevAssetBalances(assetId) + quantity)))
              .copy(issuedAssets = issuedAssets.updated(assetId, issuedAssets(assetId) + quantity))
          case TransferTransaction(assetIdOpt, sender, recipient, amount, timestamp, _, fee, _, _) =>
            val recipientAcc = accountOrAliasToAccount(recipient)
            assetIdOpt match {
              case Some(assetId) =>
                val senderWavesBalance = wavesBalances(sender.toAccount)
                val senderAssetBalance = assetsBalances(sender.toAccount)(assetId)
                val recipientAssetBalance = assetsBalances(recipientAcc)(assetId)
                r.copy(wavesBalances = wavesBalances.updated(sender.toAccount, Balances(senderWavesBalance.normal - fee, senderWavesBalance.effective - fee)))
                  .copy(assetsBalances = assetsBalances.updated(recipientAcc, assetsBalances(recipientAcc).updated(assetId, recipientAssetBalance + amount)))
                  .copy(assetsBalances = assetsBalances.updated(sender.toAccount, assetsBalances(sender.toAccount).updated(assetId, senderAssetBalance - amount)))
              case None =>
                val senderBalance = wavesBalances(sender.toAccount)
                val recipientBalance = wavesBalances(recipientAcc)
                r.copy(
                  wavesBalances.updated(recipientAcc, Balances(recipientBalance.normal + amount, recipientBalance.effective + amount))
                    .updated(sender.toAccount, Balances(senderBalance.normal - amount - fee, senderBalance.effective - amount - fee)))
            }
          case BurnTransaction(sender, assetId, amount, fee, _, _) =>
            val senderAccount = sender.toAccount
            val senderBalance = wavesBalances(senderAccount)
            r.copy(wavesBalances = wavesBalances.updated(senderAccount, Balances(senderBalance.normal - fee, senderBalance.effective - fee)))
              .copy(assetsBalances = assetsBalances.updated(senderAccount, assetsBalances(senderAccount) + (assetId -> (assetsBalances(senderAccount)(assetId) - amount))))
              .copy(issuedAssets = issuedAssets.updated(assetId, issuedAssets(assetId) + amount))
          case CreateAliasTransaction(sender, alias, fee, _, _) =>
            r.copy(aliases = aliases.updated(alias.stringRepr, sender.toAccount))
          case LeaseTransaction(sender, amount, fee, _, recipient, _) =>
            val prevSenderBalance = wavesBalances(sender.toAccount)
            val recipientAccount = accountOrAliasToAccount(recipient)
            val prevRecipientBalance = wavesBalances(recipientAccount)
            r.copy(wavesBalances = wavesBalances.updated(sender.toAccount, Balances(prevSenderBalance.normal - fee, prevSenderBalance.effective - amount - fee))
              .updated(recipientAccount, Balances(prevRecipientBalance.normal, prevRecipientBalance.effective + amount)))
          case LeaseCancelTransaction(sender, leaseId, fee, _, _) =>
            val lease = leases(leaseId)
            val prevSenderBalance = wavesBalances(sender.toAccount)
            val recipientAccount = accountOrAliasToAccount(lease.recipient)
            val prevRecipientBalance = wavesBalances(recipientAccount)
            r.copy(wavesBalances = wavesBalances.updated(sender.toAccount, Balances(prevSenderBalance.normal - fee, prevSenderBalance.effective + lease.amount - fee))
              .updated(recipientAccount, Balances(prevRecipientBalance.normal, prevRecipientBalance.effective - lease.amount)))
          case ExchangeTransaction(buy, sell, price, amount, buyMatcherFee, sellMatcherFee, fee, _, _) =>
            val buyer = buy.senderPublicKey.toAccount
            val seller = sell.senderPublicKey.toAccount
            val matcher = buy.matcherPublicKey.toAccount
            val amountAsset = buy.assetPair.amountAsset
            val priceAsset = buy.assetPair.priceAsset
            val afterSell = {
              val prevWavesBuyerBalance = wavesBalances(buyer)
              val prevAssetBuyerBalances = assetsBalances(buyer)
              val prevWavesSellerBalance = wavesBalances(seller)
              val prevAssetSellerBalances = assetsBalances(seller)
              priceAsset match {
                case None =>
                  r.copy(wavesBalances = wavesBalances.updated(seller, Balances(prevWavesSellerBalance.normal - sell.matcherFee - price * amount, prevWavesSellerBalance.effective - sell.matcherFee - price * amount)))
                case Some(assetId) =>
                  r.copy(assetsBalances = assetsBalances.updated(seller, prevAssetSellerBalances.updated(assetId, prevAssetSellerBalances(assetId) - price * amount)))
                    .copy(wavesBalances = wavesBalances.updated(seller, Balances(prevWavesSellerBalance.normal - sell.matcherFee - price * amount, prevWavesSellerBalance.effective - sell.matcherFee - price * amount)))
              }
            }
            val afterBuyAndSell = {
              val prevWavesBuyerBalance = afterSell.wavesBalances(buyer)
              val prevAssetBuyerBalances = afterSell.assetsBalances(buyer)
              val prevWavesSellerBalance = afterSell.wavesBalances(seller)
              val prevAssetSellerBalances = afterSell.assetsBalances(seller)
              amountAsset match {
                case None =>
                  afterSell.copy(wavesBalances = wavesBalances.updated(seller, Balances(prevWavesSellerBalance.normal - sell.matcherFee - price * amount, prevWavesSellerBalance.effective - sell.matcherFee - price * amount)))
                case Some(assetId) =>
                  afterSell.copy(assetsBalances = assetsBalances.updated(seller, prevAssetSellerBalances.updated(assetId, prevAssetSellerBalances(assetId) - price * amount))) .copy(wavesBalances = wavesBalances.updated(seller, Balances(prevWavesSellerBalance.normal - sell.matcherFee - price * amount, prevWavesSellerBalance.effective - sell.matcherFee - price * amount)))
              }
            }
            afterBuyAndSell
        }
    }
  }
}
