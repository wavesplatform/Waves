package com.wavesplatform.matcher.api

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.model.{StatusCodes => C, _}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.Order
import play.api.libs.json.{JsNull, JsValue, Json}
import com.wavesplatform.utils.byteStrWrites

abstract class MatcherResponse(val statusCode: StatusCode, val json: JsValue) {
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
    fromStatusCodeAndValue[StatusCode, JsValue].compose(mr => mr.statusCode -> mr.json)

  implicit def tuple2MatcherResponse(v: (StatusCode, String)): MatcherResponse = MatcherResponse(v._1, v._2)

  def apply(code: StatusCode, message: String): MatcherResponse = SimpleResponse(code, message)
}

case class SimpleResponse(code: StatusCode, message: String) extends MatcherResponse(code, message)

case class NotImplemented(message: String) extends MatcherResponse(C.NotImplemented, message)

case object InvalidSignature extends MatcherResponse(C.BadRequest, "Invalid signature")

case object OperationTimedOut
    extends MatcherResponse(C.InternalServerError, Json.obj("status" -> "OperationTimedOut", "message" -> "Operation is timed out, please try later"))

case class OrderAccepted(order: Order) extends MatcherResponse(C.OK, Json.obj("status" -> "OrderAccepted", "message" -> order.json()))

case class OrderRejected(message: String) extends MatcherResponse(C.BadRequest, Json.obj("status" -> "OrderRejected", "message" -> message))

case class OrderCanceled(orderId: ByteStr) extends MatcherResponse(C.OK, Json.obj("status" -> "OrderCanceled", "orderId" -> orderId))

case class OrderDeleted(orderId: ByteStr) extends MatcherResponse(C.OK, Json.obj("status" -> "OrderDeleted", "orderId" -> orderId))

case class OrderCancelRejected(message: String)
    extends MatcherResponse(C.BadRequest, Json.obj("status" -> "OrderCancelRejected", "message" -> message))

case object OrderBookUnavailable extends MatcherResponse(C.ServiceUnavailable, "Order book is unavailable. Please contact the administrator")

case object DuringShutdown extends MatcherResponse(C.ServiceUnavailable, "System is going shutdown")
