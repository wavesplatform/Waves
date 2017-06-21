package com.wavesplatform.it.util

import scorex.account.{AccountOrAlias, Alias, PrivateKeyAccount}
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.{CreateAliasTransaction, PaymentTransaction, Transaction}

import scala.concurrent.duration._
import scala.util.Random

object TransactionGenerator {
  private val r = new Random()
  private def randomFrom[T](c: Seq[T]): Option[T] = if (c.nonEmpty) Some(c(r.nextInt(c.size))) else None

  def gen(accounts: Seq[PrivateKeyAccount], n: Int): Seq[Transaction] = {
    val issueTransactionSender = randomFrom(accounts).get
    val tradeAssetIssue = IssueTransaction.create(issueTransactionSender, "TRADE".getBytes, "Waves DEX is the best exchange ever".getBytes, 100000000, 2, reissuable = false, 100000, System.currentTimeMillis()).right.get

    def tradeAssetDistribution = {
      tradeAssetIssue +: accounts.map(acc => {
        TransferTransaction.create(Some(tradeAssetIssue.id), issueTransactionSender, acc.toAccount, 5, System.currentTimeMillis(), None, 100000, Array.fill(r.nextInt(100))(r.nextInt().toByte)).right.get
      })
    }

    val generated = (0 until n - tradeAssetDistribution.size).foldLeft((
      Seq.empty[Transaction],
      Seq.empty[IssueTransaction],
      Seq.empty[IssueTransaction],
      Seq.empty[LeaseTransaction],
      Seq.empty[CreateAliasTransaction]
    )) {
      case ((allTxsWithValid, validIssueTxs, reissuableIssueTxs, activeLeaseTransactions, aliases), _) =>
        def fee = 100000L + r.nextInt(100000)
        val txType = 2 + r.nextInt(11 - 2)
        def ts = System.currentTimeMillis()
        val tx = TransactionType(txType) match {
          case TransactionType.PaymentTransaction =>
            val sender = randomFrom(accounts).get
            val recipient = accounts(new Random().nextInt(accounts.size))
            Some(PaymentTransaction.create(sender, recipient, 1, fee, ts).right.get)
          case TransactionType.IssueTransaction =>
            val sender = randomFrom(accounts).get
            val name = new Array[Byte](10)
            val description = new Array[Byte](10)
            r.nextBytes(name)
            r.nextBytes(description)
            val reissuable = r.nextBoolean()
            Some(IssueTransaction.create(sender, name, description, 100000000 + Random.nextInt(Int.MaxValue), Random.nextInt(9).toByte, reissuable, fee, ts).right.get)
          case TransactionType.TransferTransaction =>
            val sender = randomFrom(accounts).get
            val useAlias = r.nextBoolean()
            val recipientOpt: Option[AccountOrAlias] = if (useAlias && aliases.nonEmpty) randomFrom(aliases).map(_.alias) else randomFrom(accounts).map(_.toAccount)
            val sendAsset = r.nextBoolean()
            val asset = if (sendAsset && validIssueTxs.nonEmpty) randomFrom(validIssueTxs).map(_.id) else None
            recipientOpt.map(recipient => TransferTransaction.create(asset, sender, recipient, 5, ts, None, fee, Array.fill(r.nextInt(100))(r.nextInt().toByte)).right.get)
          case TransactionType.ReissueTransaction =>
            val reissuable = r.nextBoolean()
            randomFrom(reissuableIssueTxs).map(assetTx => {
              val sender = accounts.find(_.address == assetTx.sender.address).get
              ReissueTransaction.create(sender, assetTx.id, Random.nextInt(Int.MaxValue), reissuable, fee, ts).right.get
            })
          case TransactionType.BurnTransaction =>
            randomFrom(validIssueTxs).map(assetTx => {
              val sender = accounts.find(_.address == assetTx.sender.address).get
              BurnTransaction.create(sender, assetTx.id, Random.nextInt(1000), fee, ts).right.get
            })
          case TransactionType.ExchangeTransaction =>
            val matcher = randomFrom(accounts).get
            val seller = randomFrom(accounts).get
            val pair = AssetPair(Some(tradeAssetIssue.id), None)
            val sellOrder = Order.sell(seller, matcher, pair, 100000000, 1, ts, ts + 30.days.toMillis, fee * 3)
            val buyer = randomFrom(accounts).get
            val buyOrder = Order.buy(buyer, matcher, pair, 100000000, 1, ts, ts + 1.day.toMillis, fee * 3)
            Some(ExchangeTransaction.create(matcher, buyOrder, sellOrder, 100000000, 1, 300000, 300000, fee * 3, ts).right.get)
          case TransactionType.LeaseTransaction =>
            val sender = randomFrom(accounts).get
            val useAlias = r.nextBoolean()
            val recipientOpt: Option[AccountOrAlias] = if (useAlias && aliases.nonEmpty) randomFrom(aliases.filter(_.sender != sender.toAccount)).map(_.alias) else randomFrom(accounts).map(_.toAccount).filter(_ != sender.toAccount)
            recipientOpt.map(recipient =>
              LeaseTransaction.create(sender, 1, fee * 3, ts, recipient).right.get)
          case TransactionType.LeaseCancelTransaction =>
            randomFrom(activeLeaseTransactions).map(lease => {
              val sender = accounts.find(_.address == lease.sender.address).get
              LeaseCancelTransaction.create(sender, lease.id, fee * 3, ts).right.get
            })
          case TransactionType.CreateAliasTransaction =>
            val sender = randomFrom(accounts).get
            val aliasBytes = new Array[Byte](15)
            r.nextBytes(aliasBytes)
            val aliasString = new String(aliasBytes).trim.replace("\n", "")
            Some(CreateAliasTransaction.create(sender, Alias.buildWithCurrentNetworkByte(aliasString).right.get, 100000, ts).right.get)
        }
        (tx.map(tx => allTxsWithValid :+ tx).getOrElse(allTxsWithValid),
          tx match {
            case Some(tx: IssueTransaction) => validIssueTxs :+ tx
            case _ => validIssueTxs
          },
          tx match {
            case Some(tx: IssueTransaction) if tx.reissuable => reissuableIssueTxs :+ tx
            case Some(tx: ReissueTransaction) if !tx.reissuable => reissuableIssueTxs.filter(_.id != tx.id)
            case _ => reissuableIssueTxs
          },
          tx match {
            case Some(tx: LeaseTransaction) => activeLeaseTransactions :+ tx
            case Some(tx: LeaseCancelTransaction) => activeLeaseTransactions.filter(_.id != tx.id)
            case _ => activeLeaseTransactions
          },
          tx match {
            case Some(tx: CreateAliasTransaction) => aliases :+ tx
            case _ => aliases
          }
        )
    }

    tradeAssetDistribution ++ generated._1
  }
}
