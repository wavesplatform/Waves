package com.wavesplatform.generator

import java.util.concurrent.ThreadLocalRandom

import cats.Show
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.generator.utils.Gen
import com.wavesplatform.generator.utils.Implicits.DoubleExt
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, Transaction, TxVersion}

import scala.concurrent.duration._

class SmartGenerator(settings: SmartGenerator.Settings, val accounts: Seq[KeyPair], estimator: ScriptEstimator) extends TransactionGenerator {
  private def r                                   = ThreadLocalRandom.current
  private def randomFrom[T](c: Seq[T]): Option[T] = if (c.nonEmpty) Some(c(r.nextInt(c.size))) else None

  override def next(): Iterator[Transaction] = generate(settings).iterator

  private def generate(settings: SmartGenerator.Settings): Seq[Transaction] = {
    val bank = randomFrom(accounts).get

    val fee = 0.005.waves

    val script: Script = Gen.script(settings.complexity, estimator)

    val setScripts = Range(0, settings.scripts) flatMap (_ =>
      accounts.map { i =>
        SetScriptTransaction.selfSigned(1.toByte, i, Some(script), 1.waves, System.currentTimeMillis()).explicitGet()
      }
    )

    val now = System.currentTimeMillis()
    val txs = Range(0, settings.transfers).map { i =>
      TransferTransaction
        .selfSigned(2.toByte, bank, bank.toAddress, Waves, 1.waves - 2 * fee, Waves, fee, ByteStr.empty, now + i)
        .explicitGet()
    }

    val extxs = Range(0, settings.exchange).map { i =>
      val ts = now + i

      val matcher         = randomFrom(accounts).get
      val seller          = randomFrom(accounts).get
      val buyer           = randomFrom(accounts).get
      val asset           = randomFrom(settings.assets.toSeq)
      val tradeAssetIssue = ByteStr.decodeBase58(asset.get).toOption
      val pair            = AssetPair(Waves, Asset.fromCompatId(tradeAssetIssue))
      val sellOrder = Order.sell(TxVersion.V2, seller, matcher.publicKey, pair, 100000000L, 1, ts, ts + 30.days.toMillis, 0.003.waves).explicitGet()
      val buyOrder  = Order.buy(TxVersion.V2, buyer, matcher.publicKey, pair, 100000000L, 1, ts, ts + 1.day.toMillis, 0.003.waves).explicitGet()

      ExchangeTransaction
        .signed(TxVersion.V2, matcher.privateKey, buyOrder, sellOrder, 100000000, 1, 0.003.waves, 0.003.waves, 0.011.waves, ts)
        .explicitGet()
    }

    setScripts ++ txs ++ extxs
  }

}

object SmartGenerator {
  final case class Settings(scripts: Int, transfers: Int, complexity: Boolean, exchange: Int, assets: Set[String]) {
    require(scripts >= 0)
    require(transfers >= 0)
    require(exchange >= 0)
  }

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._
      s"""
         | set-scripts = $scripts
         | transfers = $transfers
         | complexity = $complexity
         | exchange = $exchange
         | assets = $assets
      """.stripMargin
    }
  }
}
