package com.wavesplatform.generator

import java.util.concurrent.ThreadLocalRandom

import cats.Show
import cats.data._
import cats.implicits._
import com.wavesplatform.generator.Worker.Settings
import com.wavesplatform.generator.utils.{ApiRequests, GenOrderType}
import com.wavesplatform.it.util._
import org.asynchttpclient.{AsyncHttpClient, Response}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.AssetId
import scorex.transaction.assets.exchange.{AssetPair, Order}
import settings.{GeneratorSettings, MatcherNodeSettings}

import scala.concurrent.duration.{Duration, FiniteDuration, _}
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.Success
import scala.util.Failure

class Worker(workerSettings: Settings,
             generatorSettings: GeneratorSettings,
             matcherSettings: MatcherNodeSettings.Settings,
             val tradingAssets: Seq[AssetId],
             orderType: GenOrderType.Value,
             ordersCount: Int,
             client: AsyncHttpClient)(implicit ec: ExecutionContext)
    extends ApiRequests(client) {

  private type Result[T] = EitherT[Future, (Int, Throwable), T]

  def run(): Future[Unit] =
    placeOrders(1).leftMap { _ =>
      ()
    }.merge

  private val matcherPublicKey = PublicKeyAccount.fromBase58String(matcherSettings.matcherKey).right.get
  private val fee              = 0.003.waves

  private def ts = System.currentTimeMillis()

  private def r                                   = ThreadLocalRandom.current
  private def randomFrom[T](c: Seq[T]): Option[T] = if (c.nonEmpty) Some(c(r.nextInt(c.size))) else None

  def buyOrder(price: Long, amount: Long, accounts: Seq[PrivateKeyAccount]): Future[Response] = {
    val pair  = AssetPair(randomFrom(tradingAssets), None)
    val buyer = randomFrom(accounts).get
    val order = Order.buy(buyer, matcherPublicKey, pair, price, amount, ts, ts + 1.day.toMillis, fee)
    to(matcherSettings.endpoint).placeOrder(order)
  }

  def sellOrder(price: Long, amount: Long, accounts: Seq[PrivateKeyAccount]): Future[Response] = {
    val pair   = AssetPair(randomFrom(tradingAssets), None)
    val seller = randomFrom(accounts).get
    val order  = Order.sell(seller, matcherPublicKey, pair, price, amount, ts, ts + 1.day.toMillis, fee)
    to(matcherSettings.endpoint).placeOrder(order)
  }

  private def placeOrders(startStep: Int): Result[Unit] = {
    val defaultAmount = 0.0001.waves
    val defaultPrice  = 1000

    def loop(step: Int): Result[Unit] = {

      def trySend: Future[Seq[Response]] = Future.sequence {
        val result: Seq[Future[Response]] = (1 to ordersCount).map(_ =>
          orderType match {
            case GenOrderType.ActiveBuy =>
              buyOrder(defaultPrice * 100, defaultAmount, generatorSettings.validAccounts)
            case GenOrderType.ActiveSell =>
              sellOrder(defaultPrice / 100, defaultAmount, generatorSettings.validAccounts)
            case GenOrderType.Buy =>
              buyOrder(defaultPrice, defaultAmount, generatorSettings.validAccounts)
            case GenOrderType.Sell =>
              sellOrder(defaultPrice, defaultAmount, generatorSettings.validAccounts)
            case GenOrderType.InvalidAmount =>
              buyOrder(defaultPrice, defaultAmount, generatorSettings.invalidAccounts)
                .transformWith {
                  // case Failure(IllegalArgumentException) => Future.successful(())
                  case Success(x) => Future.failed(new IllegalStateException(s"Order should not be placed: ${x.getResponseBody}"))
                }
            case GenOrderType.Fake =>
              sellOrder(defaultPrice, defaultAmount, generatorSettings.fakeAccounts)
        })
        result
      }

      def xxx: EitherT[Future, Nothing, Seq[Response]] = EitherT.liftF(trySend)

      def next: Result[Unit] = {
        log.info(s"${ordersCount} transactions had been sent")
        if (step < workerSettings.iterations) {
          log.info(s" Sleeping for ${workerSettings.delay}")
          blocking {
            Thread.sleep(workerSettings.delay.toMillis)
          }
          loop(step + 1)
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
