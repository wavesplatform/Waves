package com.wavesplatform.generator

import org.slf4j.LoggerFactory
import scorex.account.{Alias, PrivateKeyAccount}
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.{CreateAliasTransaction, PaymentTransaction, Transaction, ValidationError}
import scorex.utils.LoggerFacade

import scala.concurrent.duration._
import java.util.concurrent.ThreadLocalRandom

import cats.Show
import com.wavesplatform.generator.NarrowTransactionGenerator.Settings

import scala.util.Random

class NarrowTransactionGenerator(settings: Settings,
                                 val accounts: Seq[PrivateKeyAccount]) extends TransactionGenerator {

  private def r = ThreadLocalRandom.current
  private val log = LoggerFacade(LoggerFactory.getLogger(getClass))
  private val typeGen = new DistributedRandomGenerator(settings.probabilities)

  private def randomFrom[T](c: Seq[T]): Option[T] = if (c.nonEmpty) Some(c(r.nextInt(c.size))) else None

  private def logOption[T <: Transaction](txE: Either[ValidationError, T])(implicit m: Manifest[T]): Option[T] = {
    txE match {
      case Left(e) =>
        log.warn(s"${m.runtimeClass.getName}: ${e.toString}")
        None
      case Right(tx) => Some(tx)
    }
  }

  override def next(): Iterator[Transaction] = generate(settings.transactions).toIterator

  def generate(n: Int): Seq[Transaction] = {
    val issueTransactionSender = randomFrom(accounts).get
    val tradeAssetIssue = IssueTransaction.create(issueTransactionSender, "TRADE".getBytes,
      "Waves DEX is the best exchange ever".getBytes, 100000000, 2, reissuable = false,
      100000000L + r.nextInt(100000000), System.currentTimeMillis()).right.get

    val tradeAssetDistribution = {
      tradeAssetIssue +: accounts.map(acc => {
        TransferTransaction.create(Some(tradeAssetIssue.id), issueTransactionSender, acc, 5, System.currentTimeMillis(), None, 100000, Array.fill(r.nextInt(100))(r.nextInt().toByte)).right.get
      })
    }


    val generated = (0 until (n * 1.2).toInt).foldLeft((
      Seq.empty[Transaction],
      Seq.empty[IssueTransaction],
      Seq.empty[IssueTransaction],
      Seq.empty[LeaseTransaction],
      Seq.empty[CreateAliasTransaction]
    )) {
      case ((allTxsWithValid, validIssueTxs, reissuableIssueTxs, activeLeaseTransactions, aliases), _) =>
        def moreThatStandartFee = 100000L + r.nextInt(100000)

        val txType = typeGen.getRandom

        def ts = System.currentTimeMillis()

        val tx = txType match {
          case TransactionType.PaymentTransaction =>
            val sender = randomFrom(accounts).get
            val recipient = accounts(new Random().nextInt(accounts.size))
            Some(PaymentTransaction.create(sender, recipient, r.nextInt(500000), moreThatStandartFee, ts).right.get)
          case TransactionType.IssueTransaction =>
            val sender = randomFrom(accounts).get
            val name = new Array[Byte](10)
            val description = new Array[Byte](10)
            r.nextBytes(name)
            r.nextBytes(description)
            val reissuable = r.nextBoolean()
            val amount = 100000000L + Random.nextInt(Int.MaxValue)
            logOption(IssueTransaction.create(sender, name, description, amount, Random.nextInt(9).toByte, reissuable, 100000000L + r.nextInt(100000000), ts))
          case TransactionType.TransferTransaction =>
            val useAlias = r.nextBoolean()
            val recipient = if (useAlias && aliases.nonEmpty) randomFrom(aliases).map(_.alias).get else randomFrom(accounts).get.toAddress
            val sendAsset = r.nextBoolean()
            val senderAndAssetOpt = if (sendAsset) {
              val asset = randomFrom(validIssueTxs)
              asset.map(issue => {
                val pk = accounts.find(_ == issue.sender).get
                (pk, Some(issue.id))
              })
            } else Some(randomFrom(accounts).get, None)
            senderAndAssetOpt.flatMap { case (sender, asset) =>
              logOption(TransferTransaction.create(asset, sender, recipient, r.nextInt(500000), ts, None, moreThatStandartFee,
                Array.fill(r.nextInt(100))(r.nextInt().toByte)))
            }
          case TransactionType.ReissueTransaction =>
            val reissuable = r.nextBoolean()
            randomFrom(reissuableIssueTxs).flatMap(assetTx => {
              val sender = accounts.find(_.address == assetTx.sender.address).get
              logOption(ReissueTransaction.create(sender, assetTx.id, Random.nextInt(Int.MaxValue), reissuable, moreThatStandartFee, ts))
            })
          case TransactionType.BurnTransaction =>
            randomFrom(validIssueTxs).flatMap(assetTx => {
              val sender = accounts.find(_.address == assetTx.sender.address).get
              logOption(BurnTransaction.create(sender, assetTx.id, Random.nextInt(1000), moreThatStandartFee, ts))
            })
          case TransactionType.ExchangeTransaction =>
            val matcher = randomFrom(accounts).get
            val seller = randomFrom(accounts).get
            val pair = AssetPair(None, Some(tradeAssetIssue.id))
            val sellOrder = Order.sell(seller, matcher, pair, 100000000, 1, ts, ts + 30.days.toMillis, moreThatStandartFee * 3)
            val buyer = randomFrom(accounts).get
            val buyOrder = Order.buy(buyer, matcher, pair, 100000000, 1, ts, ts + 1.day.toMillis, moreThatStandartFee * 3)
            logOption(ExchangeTransaction.create(matcher, buyOrder, sellOrder, 100000000, 1, 300000, 300000, moreThatStandartFee * 3, ts))
          case TransactionType.LeaseTransaction =>
            val sender = randomFrom(accounts).get
            val useAlias = r.nextBoolean()
            val recipientOpt = if (useAlias && aliases.nonEmpty) randomFrom(aliases.filter(_.sender != sender)).map(_.alias) else randomFrom(accounts.filter(_ != sender).map(_.toAddress))
            recipientOpt.flatMap(recipient =>
              logOption(LeaseTransaction.create(sender, 1, moreThatStandartFee * 3, ts, recipient)))
          case TransactionType.LeaseCancelTransaction =>
            randomFrom(activeLeaseTransactions).flatMap(lease => {
              val sender = accounts.find(_.address == lease.sender.address).get
              logOption(LeaseCancelTransaction.create(sender, lease.id, moreThatStandartFee * 3, ts))
            })
          case TransactionType.CreateAliasTransaction =>
            val sender = randomFrom(accounts).get
            val aliasString = NarrowTransactionGenerator.generateAlias()
            logOption(CreateAliasTransaction.create(sender, Alias.buildWithCurrentNetworkByte(aliasString).right.get, 100000, ts))
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
            case Some(tx: LeaseCancelTransaction) => activeLeaseTransactions.filter(_.id != tx.leaseId)
            case _ => activeLeaseTransactions
          },
          tx match {
            case Some(tx: CreateAliasTransaction) => aliases :+ tx
            case _ => aliases
          }
        )
    }

    tradeAssetDistribution ++ generated._1.take(n - tradeAssetDistribution.size)
  }
}

object NarrowTransactionGenerator {

  case class Settings(transactions: Int, probabilities: Map[TransactionType.Value, Double])

  private val minAliasLength = 4
  private val maxAliasLength = 30
  private val aliasAlphabet = "-.0123456789@_abcdefghijklmnopqrstuvwxyz".toVector

  def generateAlias(): String = {
    val len = Random.nextInt(maxAliasLength - minAliasLength) + minAliasLength
    Random.shuffle(aliasAlphabet).take(len).mkString
  }

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._
      s"""transactions per iteration: $transactions
          |probabilities:
          |  ${probabilities.mkString("\n  ")}""".stripMargin
    }
  }

}