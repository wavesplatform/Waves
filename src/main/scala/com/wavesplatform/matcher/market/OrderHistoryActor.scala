package com.wavesplatform.matcher.market

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.{MatcherResponse, StatusCodeMatcherResponse}
import com.wavesplatform.matcher.market.OrderHistoryActor.{ExpirableOrderHistoryRequest, _}
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model._
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.state.ByteStr
import com.wavesplatform.utx.UtxPool
import kamon.Kamon
import org.iq80.leveldb.DB
import play.api.libs.json._
import scorex.account.Address
import scorex.transaction.AssetAcc
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.utils.NTP
import scorex.wallet.Wallet

import scala.concurrent.duration._
import scala.language.postfixOps

class OrderHistoryActor(db: DB, val settings: MatcherSettings, val utxPool: UtxPool, val wallet: Wallet) extends Actor with OrderValidator {

  val orderHistory = new OrderHistory(db, settings)

  private val timer          = Kamon.timer("matcher.order-history")
  private val addedTimer     = timer.refine("event" -> "added")
  private val executedTimer  = timer.refine("event" -> "executed")
  private val cancelledTimer = timer.refine("event" -> "cancelled")

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[OrderAdded])
    context.system.eventStream.subscribe(self, classOf[OrderExecuted])
    context.system.eventStream.subscribe(self, classOf[OrderCanceled])
  }

  def processExpirableRequest(r: Any): Unit = r match {
    case ValidateOrder(o, _) =>
      sender() ! ValidateOrderResult(o.id(), validateNewOrder(o))
    case req: DeleteOrderFromHistory =>
      deleteFromOrderHistory(req)
    case GetTradableBalance(assetPair, addr, _) =>
      sender() ! getPairTradableBalance(assetPair, addr)
  }

  override def receive: Receive = {
    case req: ExpirableOrderHistoryRequest =>
      if (NTP.correctedTime() - req.ts < RequestTTL) {
        processExpirableRequest(req)
      }
    case ev: OrderAdded =>
      addedTimer.measure(orderHistory.orderAccepted(ev))
    case ev: OrderExecuted =>
      executedTimer.measure(orderHistory.orderExecuted(ev))
    case ev: OrderCanceled =>
      cancelledTimer.measure(orderHistory.orderCanceled(ev))
    case ForceCancelOrderFromHistory(id) =>
      forceCancelOrder(id)
  }

  def forceCancelOrder(id: ByteStr): Unit = {
    val maybeOrder = orderHistory.order(id)
    for (o <- maybeOrder) {
      val oi = orderHistory.orderInfo(id)
      orderHistory.orderCanceled(OrderCanceled(LimitOrder.limitOrder(o.price, oi.remaining, oi.remainingFee, o), unmatchable = false))
    }
    sender ! maybeOrder
  }

  def getPairTradableBalance(assetPair: AssetPair, acc: Address): GetTradableBalanceResponse = {
    val bal = (for {
      amountAcc <- Right(AssetAcc(acc, assetPair.amountAsset))
      priceAcc  <- Right(AssetAcc(acc, assetPair.priceAsset))
      amountBal <- Right(getTradableBalance(amountAcc))
      priceBal  <- Right(getTradableBalance(priceAcc))
    } yield (amountBal, priceBal)) match {
      case Left(_)  => (0L, 0L)
      case Right(b) => b
    }

    GetTradableBalanceResponse(
      Map(
        assetPair.amountAssetStr -> bal._1,
        assetPair.priceAssetStr  -> bal._2
      ))
  }

  def deleteFromOrderHistory(req: DeleteOrderFromHistory): Unit = {
    orderHistory.orderInfo(req.id).status match {
      case LimitOrder.Filled(_) | LimitOrder.Cancelled(_) =>
        orderHistory.deleteOrder(req.address, req.id)
        sender() ! OrderDeleted(req.id)
      case _ =>
        sender() ! StatusCodeMatcherResponse(StatusCodes.BadRequest, "Order couldn't be deleted")
    }
  }
}

object OrderHistoryActor {
  val RequestTTL: Int                          = 5 * 1000
  val UpdateOpenPortfolioDelay: FiniteDuration = 30 seconds

  def name: String = "OrderHistory"

  def props(db: DB, settings: MatcherSettings, utxPool: UtxPool, wallet: Wallet): Props =
    Props(new OrderHistoryActor(db, settings, utxPool, wallet))

  sealed trait ExpirableOrderHistoryRequest {
    def ts: Long
  }

  case class DeleteOrderFromHistory(assetPair: AssetPair, address: Address, id: ByteStr, ts: Long) extends ExpirableOrderHistoryRequest

  case class ValidateOrder(order: Order, ts: Long) extends ExpirableOrderHistoryRequest

  case class ValidateOrderResult(validatedOrderId: ByteStr, result: Either[GenericError, Order])

  case class ForceCancelOrderFromHistory(orderId: ByteStr)

  case class OrderDeleted(orderId: ByteStr) extends MatcherResponse(StatusCodes.OK, Json.obj("status" -> "OrderDeleted", "orderId" -> orderId))

  case class GetTradableBalance(assetPair: AssetPair, address: Address, ts: Long) extends ExpirableOrderHistoryRequest

  case class GetTradableBalanceResponse(balances: Map[String, Long])
      extends MatcherResponse(StatusCodes.OK, JsObject(balances.map { case (k, v) => (k, JsNumber(v)) }))
}
