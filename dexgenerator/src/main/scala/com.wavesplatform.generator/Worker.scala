package com.wavesplatform.generator

import java.util.concurrent.ThreadLocalRandom

import cats.Show
import cats.data._
import cats.implicits._
import com.wavesplatform.crypto
import com.wavesplatform.generator.Worker.Settings
import com.wavesplatform.generator.utils.{ApiRequests, GenOrderType}
import com.wavesplatform.it.api.{MatcherResponse, MatcherStatusResponse, OrderbookHistory, UnexpectedStatusCodeException}
import com.wavesplatform.it.util._
import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.matcher.model.LimitOrder.OrderStatus
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import org.asynchttpclient.{AsyncHttpClient, Response}
import play.api.libs.json._
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.{Curve25519, PrivateKey}
import scorex.transaction.AssetId
import scorex.transaction.assets.exchange.{AssetPair, Order}
import settings.{GeneratorSettings, MatcherNodeSettings}

import scala.collection.immutable
import scala.concurrent.duration.{Duration, FiniteDuration, _}
import scala.concurrent._
import scala.util.{Failure, Success, Try}
import Worker._

class Worker(workerSettings: Settings,
             generatorSettings: GeneratorSettings,
             matcherSettings: MatcherNodeSettings.Settings,
             tradingAssets: Seq[AssetId],
             orderType: GenOrderType.Value,
             ordersCount: Int,
             client: AsyncHttpClient)(implicit ec: ExecutionContext)
  extends ApiRequests(client) {


  log.info("started worker " + orderType)

  private type Result[T] = EitherT[Future, (Int, Throwable), T]

  def run(): Future[Unit] =
    placeOrders(1).leftMap { _ =>
      ()
    }.merge

  private val matcherPublicKey = PublicKeyAccount.fromBase58String(matcherSettings.matcherKey).right.get
  private val validAccounts = generatorSettings.validAccounts
  private val invalidAccounts = generatorSettings.invalidAccounts
  private val fakeAccounts = generatorSettings.fakeAccounts

  private val fee = 0.003.waves

  private def ts = System.currentTimeMillis()

  private def r = ThreadLocalRandom.current

  private def randomFrom[T](c: Seq[T]): Option[T] = if (c.nonEmpty) Some(c(r.nextInt(c.size))) else None

  def buyOrder(price: Long, amount: Long, buyer: PrivateKeyAccount, pair: AssetPair): (Order, Future[MatcherResponse]) = {
    val order = Order.buy(buyer, matcherPublicKey, pair, price, amount, ts, ts + 1.day.toMillis, fee)
    log.info("buy " + order)
    val response = to(matcherSettings.endpoint).placeOrder(order)
    (order, response)
  }

  def sellOrder(price: Long, amount: Long, buyer: PrivateKeyAccount, pair: AssetPair): (Order, Future[MatcherResponse]) = {
    val order = Order.buy(buyer, matcherPublicKey, pair, price, amount, ts, ts + 1.day.toMillis, fee)
    log.info("buy " + order)
    val response = to(matcherSettings.endpoint).placeOrder(order)
    (order, response)
  }

  def cancelOrder(pk: PrivateKeyAccount, pair: AssetPair, orderId: String): Future[MatcherStatusResponse] = {
    val request = CancelOrderRequest(PublicKeyAccount(pk.publicKey), Base58.decode(orderId).get, Array.emptyByteArray)
    val sig = crypto.sign(pk, request.toSign)
    val signedRequest = request.copy(signature = sig)
    to(matcherSettings.endpoint).cancelOrder(pair.amountAssetStr, pair.priceAssetStr, signedRequest)
  }

  def isActive(status: String): Boolean = {
    status match {
      case "PartiallyFilled" =>
        true
      case "Accepted" =>
        true
      case _ =>
        false
    }
  }

  def cancelAllOrders(fakeAccounts: Seq[PrivateKeyAccount]): Future[Seq[MatcherStatusResponse]] = {
    val orderHistoriesPerAccount: Seq[(PrivateKeyAccount, Seq[OrderbookHistory])] = fakeAccounts.map(account =>
      account -> to(matcherSettings.endpoint).orderHistory(account).filter(o => isActive(o.status)))
    log.info(s"$orderHistoriesPerAccount")
    val cancelOrders: Seq[Future[MatcherStatusResponse]] = orderHistoriesPerAccount.flatMap(account =>
      account._2.map(o => cancelOrder(account._1, o.assetPair, o.id)))
    Future.sequence(cancelOrders).andThen {
      case x => log.info(s"ended with $x")
    }
  }

  private def placeOrders(startStep: Int): Result[Unit] = {
    val defaultAmount = 10000
    val defaultPrice = 10000

    def loop(step: Int): Result[Unit] = {

      def trySend = Future.sequence {
        val result = (1 to ordersCount).map(_ =>
          orderType match {
            case GenOrderType.ActiveBuy =>
              log.info("always active buy")
              val buyer = randomFrom(validAccounts).get
              val pair = AssetPair(randomFrom(tradingAssets.take(tradingAssets.size - 2)), None)
              buyOrder(defaultPrice / 100, defaultAmount, buyer, pair)._2
            case GenOrderType.ActiveSell =>
              log.info("always active sell")
              val seller = randomFrom(validAccounts).get
              val pair = AssetPair(randomFrom(tradingAssets.take(tradingAssets.size - 2)), None)
              sellOrder(defaultPrice / 100, defaultAmount, seller, pair)._2
            case GenOrderType.Buy =>
              log.info("buy for filling")
              val buyer = randomFrom(validAccounts).get
              val pair = AssetPair(randomFrom(tradingAssets.take(tradingAssets.size - 2)), None)
              buyOrder(defaultPrice, defaultAmount, buyer, pair)._2
            case GenOrderType.Sell =>
              log.info("sell for filling")
              val seller = randomFrom(validAccounts).get
              val pair = AssetPair(randomFrom(tradingAssets.take(tradingAssets.size - 2)), None)
              sellOrder(defaultPrice, defaultAmount, seller, pair)._2
            case GenOrderType.Cancel =>
              log.info("cancel order")
              val buyer = randomFrom(validAccounts).get
              val pair = AssetPair(randomFrom(tradingAssets.take(tradingAssets.size - 2)), None)
              val orderInfo = buyOrder(defaultPrice * 100, defaultAmount, buyer, pair)
              val placedOrder = Await.result(orderInfo._2, 30.seconds)
              cancelOrder(buyer, pair, placedOrder.message.id)
            case GenOrderType.InvalidAmount =>
              log.info("invalid amount")
              val invalidBuyer = randomFrom(invalidAccounts).get
              val pair = AssetPair(randomFrom(tradingAssets.take(tradingAssets.size - 2)), None)
              buyOrder(defaultPrice, defaultAmount, invalidBuyer, pair)._2
                .transformWith {
                  case Failure(_: IllegalArgumentException) => Future.successful(())
                  case Failure(_: UnexpectedStatusCodeException) => Future.successful(())
                  case Success(x) => Future.failed(new IllegalStateException(s"Order should not be placed: ${x}"))
                }
            case GenOrderType.Fake =>
              log.info("========start cancelling")
              cancelAllOrders(fakeAccounts).flatMap { _ =>
                log.info("=======stop cancelling")
                val seller: PrivateKeyAccount = randomFrom(fakeAccounts).get
                val pair = AssetPair(randomFrom(tradingAssets.drop(tradingAssets.size - 2)), None)
                sellOrder(defaultPrice, defaultAmount, seller, pair)._2
              }



          })
        result
      }

      def xxx: EitherT[Future, Nothing, immutable.IndexedSeq[Any]] = EitherT.liftF(trySend)

      def next: Result[Unit] = {
        log.info(s"${ordersCount} orders had been sent")
        if (step < workerSettings.iterations) {
          log.info(s" Sleeping for ${workerSettings.delay}")

          EitherT.liftF(Future {
            blocking {
              Thread.sleep(workerSettings.delay.toMillis)
            }
          }).flatMap { _ =>

            loop(step + 1)
          }
        } else {
          log.info(s"Done")
          EitherT.right(Future.successful(Right(())))
        }
      }

      xxx.flatMap(_ => next)
    }

    loop(startStep)
  }

}

object Worker {

  case class Settings(autoReconnect: Boolean, iterations: Int, delay: FiniteDuration, reconnectDelay: FiniteDuration)

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._

      s"""number of iterations: $iterations
         |delay between iterations: $delay
         |auto reconnect: ${if (autoReconnect) "enabled" else "disabled"}
         |reconnect delay: $reconnectDelay""".stripMargin

    }
  }



}
object AssetPairCreator {
  val WavesName = "WAVES"

  private def extractAssetId(a: String): Try[Option[AssetId]] = a match {
    case `WavesName` => Success(None)
    case other => ByteStr.decodeBase58(other).map(Option(_))
  }

  def createAssetPair(amountAsset: String, priceAsset: String): Try[AssetPair] =
    for {
      a1 <- extractAssetId(amountAsset)
      a2 <- extractAssetId(priceAsset)
    } yield AssetPair(a1, a2)
}