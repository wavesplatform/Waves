package com.wavesplatform.matcher.api

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.model.{StatusCodes => C, _}
import com.wavesplatform.matcher.model.OrderBookResult
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.Order
import play.api.libs.json.{JsNull, JsValue, Json}

sealed abstract class MatcherResponse(val statusCode: StatusCode, val jsonBody: String)

sealed abstract class WrappedMatcherResponse(statusCode: StatusCode, val json: JsValue) extends MatcherResponse(statusCode, Json.stringify(json)) {
  def this(code: StatusCode, message: String) =
    this(code,
         Json.obj(
           "success" -> (code == C.OK),
           "message" -> message,
           "result"  -> JsNull // For backward compatibility
         ))
}

object MatcherResponse {
  import akka.http.scaladsl.marshalling.PredefinedToResponseMarshallers._
  import com.wavesplatform.http.ApiMarshallers._

  implicit val trm: ToResponseMarshaller[MatcherResponse] =
    fromStatusCodeAndHeadersAndValue[String].compose(mr =>
      (mr.statusCode, List(headers.`Content-Type`(ContentTypes.`application/json`)), mr.jsonBody))

  implicit def tuple2MatcherResponse(v: (StatusCode, String)): MatcherResponse = MatcherResponse(v._1, v._2)

  def apply(code: StatusCode, message: String): MatcherResponse = SimpleResponse(code, message)
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

case class OrderDeleted(orderId: ByteStr) extends WrappedMatcherResponse(C.OK, Json.obj("status" -> "OrderDeleted", "orderId" -> orderId))

case class OrderCancelRejected(message: String)
    extends WrappedMatcherResponse(C.BadRequest, Json.obj("status" -> "OrderCancelRejected", "message" -> message))

case object OrderBookUnavailable extends WrappedMatcherResponse(C.ServiceUnavailable, "Order book is unavailable. Please contact the administrator")

case object DuringShutdown extends WrappedMatcherResponse(C.ServiceUnavailable, "System is going shutdown")

case class GetOrderBookResponse(orderBookResult: OrderBookResult) extends MatcherResponse(C.OK, OrderBookResult.toJson(orderBookResult))
