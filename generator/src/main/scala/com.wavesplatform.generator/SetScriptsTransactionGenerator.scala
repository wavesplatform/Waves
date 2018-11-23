package com.wavesplatform.generator

import java.util.concurrent.ThreadLocalRandom

import cats.Show
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.generator.utils.Gen
import com.wavesplatform.it.util._
import com.wavesplatform.state._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.transfer.{TransferTransactionV1, TransferTransactionV2}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransactionV2, OrderV1, OrderV2}

import scala.concurrent.duration._

class SetScriptsTransactionGenerator(settings: SetScriptsTransactionGenerator.Settings, val accounts: Seq[PrivateKeyAccount])
    extends TransactionGenerator {
  private def r                                   = ThreadLocalRandom.current
  private def randomFrom[T](c: Seq[T]): Option[T] = if (c.nonEmpty) Some(c(r.nextInt(c.size))) else None
  def moreThatStandartFee                         = 700000L + r.nextInt(100000)

  def ts = System.currentTimeMillis()

  override def next(): Iterator[Transaction] = {
    generate(settings).toIterator
  }

  private def generate(settings: SetScriptsTransactionGenerator.Settings): Seq[Transaction] = {
    val issueTransactionSender = randomFrom(accounts).get
    val tradeAssetIssue = IssueTransactionV1
      .selfSigned(
        issueTransactionSender,
        "TRADE".getBytes,
        "Waves DEX is the best exchange ever".getBytes,
        100000000,
        2,
        reissuable = false,
        100000000L + r.nextInt(100000000),
        System.currentTimeMillis()
      )
      .right
      .get

    val tradeAssetDistribution = {
      tradeAssetIssue +: accounts.map(acc => {
        TransferTransactionV1
          .selfSigned(Some(tradeAssetIssue.id()),
                      issueTransactionSender,
                      acc,
                      5,
                      System.currentTimeMillis(),
                      None,
                      100000,
                      Array.fill(r.nextInt(100))(r.nextInt().toByte))
          .explicitGet()
      })
    }

    val bank = accounts.head

    val fee = 0.005.waves

    val script: Script = Gen.script(settings.complexity)

    val setScripts = Range(0, settings.scripts).map { _ =>
      accounts.map { i =>
        SetScriptTransaction.selfSigned(1, i, Some(script), 1.waves, System.currentTimeMillis()).explicitGet()
      }
    }.flatten

    val txs = Range(0, settings.transfers).map { i =>
      TransferTransactionV2
        .selfSigned(2, None, bank, bank, 1.waves - 2 * fee - i, System.currentTimeMillis(), None, fee, Array.emptyByteArray)
        .explicitGet()
    }

    val extxs = Range(0, settings.exchange).map { i =>
      val matcher = randomFrom(accounts).get
      val seller  = randomFrom(accounts).get
//      val pair    = AssetPair(None, Some(tradeAssetIssue.id()))
      val tradeAssetIssue = ByteStr.decodeBase58("7jms83saDELNbC63ShdAy8NzDwRyy4D6s5LDCDAYXm1E").toOption
      val pair            = AssetPair(None, tradeAssetIssue)
      // XXX generate order version
      val sellOrder = OrderV2.sell(seller, matcher, pair, 100000000, 1, ts, ts + 30.days.toMillis, moreThatStandartFee * 3)
      val buyer     = randomFrom(accounts).get
      val buyOrder  = OrderV2.buy(buyer, matcher, pair, 100000000, 1, ts, ts + 1.day.toMillis, moreThatStandartFee * 3)
      ExchangeTransactionV2.create(matcher, buyOrder, sellOrder, 100000000, 1, 300000, 300000, moreThatStandartFee * 3, ts).explicitGet()
    }

    /*tradeAssetIssue ++ tradeAssetIssue ++ tradeAssetDistribution ++ */
    tradeAssetDistribution ++ setScripts ++ txs ++ extxs
  }

}

object SetScriptsTransactionGenerator {
  final case class Settings(scripts: Int, transfers: Int, complexity: Boolean, exchange: Int) {
    require(scripts > 0)
    require(transfers >= 0)
  }

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._
      s"""
         | set-scripts = $scripts
         | transfers = $transfers
         | complexity = $complexity
         | exchange = $exchange
      """.stripMargin
    }

  }
}
