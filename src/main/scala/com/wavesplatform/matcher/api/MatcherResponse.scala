package com.wavesplatform.matcher.api

import akka.http.scaladsl.marshalling.{Marshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.{StatusCodes => C, _}
import akka.util.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.error.MatcherError
import com.wavesplatform.matcher.model.OrderBookResult
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.utils.byteStrWrites
import play.api.libs.json._

sealed abstract class MatcherResponse(val statusCode: StatusCode) {
  def content: String
}

object MatcherResponse {
  implicit val trm: ToResponseMarshaller[MatcherResponse] = Marshaller.opaque { x =>
    HttpResponse(
      x.statusCode,
      entity = HttpEntity.Strict(ContentTypes.`application/json`, ByteString(x.content))
    )
  }
}

sealed abstract class WrappedMatcherResponse(code: StatusCode, val json: JsObject) extends MatcherResponse(code) {
  def this(code: StatusCode, error: MatcherError) = this(code, error.json)

  override val content: String = Json.stringify(
    Json
      .obj(
        "success" -> (code match {
          case _: C.Success => true
          case _            => false
        }),
        "status" -> getClass.getSimpleName.replace("$", ""),
        "result" -> JsNull // For backward compatibility
      )
      .deepMerge(json)
  )
}

case class SimpleResponse(code: StatusCode, message: String) extends WrappedMatcherResponse(code, Json.obj("message"       -> message))
case object AlreadyProcessed                                 extends WrappedMatcherResponse(C.Accepted, Json.obj("message" -> "This event has been already processed"))
case class OrderAccepted(order: Order)                       extends WrappedMatcherResponse(C.OK, Json.obj("message"       -> order.json()))
case class OrderCanceled(orderId: ByteStr)                   extends WrappedMatcherResponse(C.OK, Json.obj("orderId"       -> orderId))
case class OrderDeleted(orderId: ByteStr)                    extends WrappedMatcherResponse(C.OK, Json.obj("orderId"       -> orderId))
case class GetOrderBookResponse(orderBookResult: OrderBookResult) extends MatcherResponse(C.OK) {
  override def content: String = OrderBookResult.toJson(orderBookResult)
}
case class BatchCancelCompleted(orders: Map[Order.Id, WrappedMatcherResponse])
    extends WrappedMatcherResponse(C.OK, Json.obj("message" -> Json.arr(orders.values.map(_.json))))

case class OrderRejected(error: MatcherError)        extends WrappedMatcherResponse(C.BadRequest, error)
case class OrderCancelRejected(error: MatcherError)  extends WrappedMatcherResponse(C.BadRequest, error)
case object CancelRequestInvalidSignature            extends WrappedMatcherResponse(C.BadRequest, MatcherError.CancelRequestInvalidSignature)
case class NotImplemented(error: MatcherError)       extends WrappedMatcherResponse(C.NotImplemented, error)
case object OperationTimedOut                        extends WrappedMatcherResponse(C.InternalServerError, MatcherError.OperationTimedOut)
case class OrderBookUnavailable(error: MatcherError) extends WrappedMatcherResponse(C.ServiceUnavailable, error)
case object DuringStart                              extends WrappedMatcherResponse(C.ServiceUnavailable, MatcherError.MatcherIsStarting)
case object DuringShutdown                           extends WrappedMatcherResponse(C.ServiceUnavailable, MatcherError.MatcherIsStopping)
