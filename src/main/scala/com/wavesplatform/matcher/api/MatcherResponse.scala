package com.wavesplatform.matcher.api

import akka.http.scaladsl.marshalling.{Marshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.{StatusCodes => C, _}
import akka.util.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.model.OrderBookResult
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.utils.byteStrWrites
import play.api.libs.json.{JsNull, JsValue, Json}

sealed abstract class MatcherResponse(val statusCode: StatusCode, val content: String)

object MatcherResponse {
  implicit val trm: ToResponseMarshaller[MatcherResponse] = Marshaller.opaque { x =>
    HttpResponse(
      x.statusCode,
      entity = HttpEntity.Strict(ContentTypes.`application/json`, ByteString(x.content))
    )
  }
}

sealed abstract class WrappedMatcherResponse(statusCode: StatusCode, val json: JsValue) extends MatcherResponse(statusCode, Json.stringify(json)) {
  def this(code: StatusCode, message: String) =
    this(code,
         Json.obj(
           "success" -> (code match {
             case _: C.Success => true
             case _            => false
           }),
           "message" -> message,
           "result"  -> JsNull // For backward compatibility
         ))
}

case class SimpleResponse(code: StatusCode, message: String) extends WrappedMatcherResponse(code, message)

case class NotImplemented(message: String) extends WrappedMatcherResponse(C.NotImplemented, message)

case object InvalidSignature extends WrappedMatcherResponse(C.BadRequest, "Invalid signature")

case object OperationTimedOut
    extends WrappedMatcherResponse(C.InternalServerError,
                                   Json.obj("status" -> "OperationTimedOut", "message" -> "Operation is timed out, please try later"))

case class OrderAccepted(order: Order) extends WrappedMatcherResponse(C.OK, Json.obj("status" -> "OrderAccepted", "message" -> order.json()))

case class OrderRejected(message: String) extends WrappedMatcherResponse(C.BadRequest, Json.obj("status" -> "OrderRejected", "message" -> message))

case class OrderCanceled(orderId: ByteStr) extends WrappedMatcherResponse(C.OK, Json.obj("status" -> "OrderCanceled", "orderId" -> orderId))

// TODO check input wasn't changed
case class BatchCancelCompleted(orders: Map[Order.Id, WrappedMatcherResponse])
    extends WrappedMatcherResponse(C.OK, Json.obj("status" -> "BatchCancelCompleted", "message" -> Json.arr(orders.values.map(_.json))))

case class OrderDeleted(orderId: ByteStr) extends WrappedMatcherResponse(C.OK, Json.obj("status" -> "OrderDeleted", "orderId" -> orderId))

case class OrderCancelRejected(message: String)
    extends WrappedMatcherResponse(C.BadRequest, Json.obj("status" -> "OrderCancelRejected", "message" -> message))

case object OrderBookUnavailable extends WrappedMatcherResponse(C.ServiceUnavailable, "Order book is unavailable. Please contact the administrator")

case object DuringStart extends WrappedMatcherResponse(C.ServiceUnavailable, "System is starting")

case object DuringShutdown extends WrappedMatcherResponse(C.ServiceUnavailable, "System is shutting down")

case class GetOrderBookResponse(orderBookResult: OrderBookResult) extends MatcherResponse(C.OK, OrderBookResult.toJson(orderBookResult))

case object AlreadyProcessed extends WrappedMatcherResponse(C.Accepted, "This event has been already processed")
