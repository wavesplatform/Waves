package com.wavesplatform.generator

import java.util.concurrent.ThreadLocalRandom

import cats.Show
import com.wavesplatform.generator.utils.GenOrderType
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.AsyncHttpApi._
import org.slf4j.LoggerFactory
import scorex.account.PrivateKeyAccount
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.utils.LoggerFacade
import settings.OrdersSettings.Settings

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random

class OrdersGenerator(settings: Settings,
                      val accounts: Seq[PrivateKeyAccount], val tradingAssets: Seq[AssetId], matcher: Node) extends OrdersGenerator {

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

  override def next(): Iterator[Unit] = generate(settings.orders).toIterator

  def generate(n: Int): Seq[Order] = {
    val issueTransactionSender = randomFrom(accounts).get
    val tradeAssetIssue = IssueTransaction.create(issueTransactionSender, "TRADE".getBytes,
      "Waves DEX is the best exchange ever".getBytes, 100000000, 2, reissuable = true,
      100000000L + r.nextInt(100000000), System.currentTimeMillis()).right.get

    val tradeAssetDistribution = {
      tradeAssetIssue +: accounts.map(acc => {
        TransferTransaction.create(Some(tradeAssetIssue.id()), issueTransactionSender, acc, 5, System.currentTimeMillis(), None, 100000, Array.fill(r.nextInt(100))(r.nextInt().toByte)).right.get
      })
    }


    val generated = (0 until (n * 1.2).toInt).foldLeft(Seq.empty[Future[Any]]) {
      case (orders, _) =>
        def moreThatStandartFee = 100000L + r.nextInt(100000)

        val orderType = typeGen.getRandom

        val defaultAmount = 1
        val defaultPrice = 10000

        val matcherPublickKey = matcher.publicKey

        def ts = System.currentTimeMillis()

        val order = orderType match {
          case GenOrderType.OrderBuy =>
            val pair = AssetPair(randomFrom(tradingAssets), None)
            val buyer = randomFrom(accounts).get
            val o = Order.buy(buyer, matcherPublickKey, pair, defaultPrice / 5, defaultAmount, ts, ts + 1.day.toMillis, moreThatStandartFee * 3)
            matcher.placeOrder(o)
          case GenOrderType.OrderSell =>
            val seller = randomFrom(accounts).get
            val pair = AssetPair(randomFrom(tradingAssets), None)
            val o = Order.sell(seller, matcherPublickKey, pair, defaultPrice, defaultAmount, ts, ts + 30.days.toMillis, moreThatStandartFee * 3)
            matcher.placeOrder(o)
          case GenOrderType.ActiveBuy =>
            val pair = AssetPair(randomFrom(tradingAssets), None)
            val buyer = randomFrom(accounts).get
            val o = Order.buy(buyer, matcherPublickKey, pair, defaultPrice / 5, defaultAmount, ts, ts + 1.day.toMillis, moreThatStandartFee * 3)
            matcher.placeOrder(o)
          case GenOrderType.ActiveSell =>
            val seller = randomFrom(accounts).get
            val pair = AssetPair(randomFrom(tradingAssets), None)
            val o = Order.sell(seller, matcherPublickKey, pair, 5 * defaultPrice, defaultAmount, ts, ts + 30.days.toMillis, moreThatStandartFee * 3)
            matcher.placeOrder(o)
          case GenOrderType.InvalidAmountOrder =>
            val seller = randomFrom(accounts).get
            val pair = AssetPair(randomFrom(tradingAssets), None)
            val o = Order.sell(seller, matcherPublickKey, pair, 5000 * defaultPrice, 5000 * defaultAmount, ts, ts + 30.days.toMillis, moreThatStandartFee * 3)
            matcher.placeOrder(o)

        }
        log.trace(s"Place $orderType order: \n$order")
        orders :+ order
    }
  }
}

object OrdersGenerator {

  case class Settings(orders: Int, assets: String, probabilities: Map[GenOrderType.Value, Double])

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
      s"""orders per iteration: $orders
         |orderbooks: $assets
         |probabilities:
         |  ${probabilities.mkString("\n  ")}""".stripMargin
    }
  }

}