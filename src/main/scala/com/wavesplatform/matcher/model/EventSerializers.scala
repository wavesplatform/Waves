package com.wavesplatform.matcher.model

import java.io.NotSerializableException
import akka.serialization._
import com.wavesplatform.matcher.market.OrderBookActor.Snapshot
import com.wavesplatform.matcher.market.MatcherActor.OrderBookCreated
import com.wavesplatform.matcher.model.Events._
import com.wavesplatform.matcher.model.MatcherModel.{Level, Price}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import scorex.transaction.AssetId
import scorex.transaction.assets.exchange.OrderJson._
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scala.collection.immutable.TreeMap
import com.wavesplatform.matcher.market.{MatcherActor, OrderBookActor}
import scorex.crypto.encode.Base58

class MatcherSerializer extends SerializerWithStringManifest {
  import MatcherSerializer._
  override def identifier: Int = id
  override def manifest(o: AnyRef): String = o match {
    case _: OrderBookActor.Snapshot => Manifest.Snapshot
    case _: MatcherActor.OrderBookCreated => Manifest.OrderBookCreated
    case _: OrderAdded => Manifest.OrderAdded
    case _: OrderExecuted => Manifest.OrderExecuted
    case _: OrderCanceled => Manifest.OrderCancelled
  }

  override def toBinary(o: AnyRef): Array[Byte] = Json.stringify(o match {
    case s: OrderBookActor.Snapshot => snapshotFormat.writes(s)
    case obc: MatcherActor.OrderBookCreated => orderBookCreatedFormat.writes(obc)
    case oa: OrderAdded => orderAddedFormat.writes(oa)
    case oe: OrderExecuted => orderExecutedFormat.writes(oe)
    case oc: OrderCanceled => orderCancelledFormat.writes(oc)
  }).getBytes

  override def fromBinary(bytes: Array[Byte], manifest: String): AnyRef = manifest match {
    case Manifest.Snapshot => snapshotFormat.reads(Json.parse(bytes)).get
    case Manifest.OrderBookCreated => orderBookCreatedFormat.reads(Json.parse(bytes)).get
    case Manifest.OrderAdded => orderAddedFormat.reads(Json.parse(bytes)).get
    case Manifest.OrderExecuted => orderExecutedFormat.reads(Json.parse(bytes)).get
    case Manifest.OrderCancelled => orderCancelledFormat.reads(Json.parse(bytes)).get
    case _ => throw new NotSerializableException(manifest)
  }
}

object MatcherSerializer {
  private[MatcherSerializer] val id = 4001

  private[MatcherSerializer] object Manifest {
    val Snapshot = "snapshot"
    val OrderBookCreated = "orderBookCreated"
    val OrderAdded = "event.OrderAdded"
    val OrderExecuted = "event.OrderExecuted"
    val OrderCancelled = "event.OrderCancelled"
  }

  implicit val byteArrayWrites: Format[Option[Array[Byte]]] = Format(
    Reads {
      case JsString("") => JsSuccess(Option.empty[Array[Byte]])
      case JsString(v) => Base58.decode(v).map(b => JsSuccess(Option(b))).getOrElse(JsError())
      case _ => JsError()
    },
    Writes(_.fold(JsString(""))(b => JsString(Base58.encode(b))))
  )

  private def zzz(lo: LimitOrder) = (lo.price, lo.amount, lo.order)
  private def fmt[T <: LimitOrder](f1: (Long, Long, Order) => T): Format[T] =
    ((__ \ "price").format[Long] and
    (__ \ "amount").format[Long] and
    (__ \ "order").format[Order])(f1, zzz)

  implicit val limitFormat: Format[LimitOrder] = fmt[LimitOrder](LimitOrder.limitOrder)
  implicit val buyFormat: Format[BuyLimitOrder] = fmt[BuyLimitOrder](BuyLimitOrder.apply)
  implicit val sellFormat: Format[SellLimitOrder] = fmt[SellLimitOrder](SellLimitOrder.apply)

  implicit val orderMapWrites = new Writes[Map[Price, Level[LimitOrder]]] {
    def writes(tree: Map[Price, Level[LimitOrder]]): JsValue =
      JsObject(tree.map { case (k, v) =>
        k.toString -> JsArray(v.map( o => Json.toJson(o)))
      })
  }

  implicit val buyOrderTreeMapReads = new Reads[TreeMap[Price, Level[BuyLimitOrder]]] {
    override def reads(jv: JsValue): JsResult[TreeMap[Price, Level[BuyLimitOrder]]] = {
      val a = jv.as[Map[String, Level[BuyLimitOrder]]].map { case(k, v) => (k.toLong, v) }
      JsSuccess(TreeMap.empty[Price, Level[BuyLimitOrder]] ++ a)
    }
  }

  implicit val sellOrderTreeMapReads = new Reads[TreeMap[Price, Level[SellLimitOrder]]] {
    override def reads(jv: JsValue): JsResult[TreeMap[Price, Level[SellLimitOrder]]] = {
      val a = jv.as[Map[String, Level[SellLimitOrder]]].map { case(k, v) => (k.toLong, v) }
      JsSuccess(TreeMap.empty[Price, Level[SellLimitOrder]] ++ a)
    }
  }

  implicit val orderBookFormat = (
    (__ \ "bids").format[TreeMap[Price, Level[BuyLimitOrder]]] and
    (__ \ "asks").format[TreeMap[Price, Level[SellLimitOrder]]])(OrderBook.apply, unlift(OrderBook.unapply))

  val orderAddedFormat = Format(
    (__ \ "o").read[LimitOrder].map(OrderAdded),
    Writes[OrderAdded](oa => Json.obj("o" -> oa.order))
  )

  val orderExecutedFormat = (
    (__ \ "o1").format[LimitOrder] and
    (__ \ "o2").format[LimitOrder])(OrderExecuted.apply, unlift(OrderExecuted.unapply))

  val orderCancelledFormat = Format(
    (__ \ "o").read[LimitOrder].map(OrderCanceled),
    Writes[OrderCanceled](oc => Json.obj("o" -> oc.limitOrder))
  )

  private def mkOrderBookCreated(a1: Option[AssetId], a2: Option[AssetId]) = OrderBookCreated(AssetPair(a1, a2))
  private def orderBookToPair(obc: OrderBookCreated) = (obc.pair.first, obc.pair.second)

  implicit val orderBookCreatedFormat: Format[OrderBookCreated] = (
    (__ \ "a1").format[Option[Array[Byte]]] and
    (__ \ "a2").format[Option[Array[Byte]]])(mkOrderBookCreated, orderBookToPair)

  implicit val tuple2Format = new Format[(Long, Long)] {
    def writes(o: (Long, Long)): JsValue = Json.arr(o._1, o._2)

    def reads(json: JsValue): JsResult[(Long, Long)] = {
      val a = json.as[JsArray].value
      JsSuccess((a.head.as[Long], a(1).as[Long]))
    }
  }

  implicit val cacheFormat = new Format[Map[String, (Long, Long)]] {
    def writes(cache: Map[String, (Long, Long)]): JsValue =
      JsObject(cache.mapValues(v => Json.arr(v._1, v._2)))

    def reads(jv: JsValue): JsResult[Map[String, (Long, Long)]] =
      JsSuccess(jv.as[Map[String, (Long, Long)]])
  }

  implicit val snapshotFormat: Format[Snapshot] = (
    (JsPath \ "o").format[OrderBook] and
      (JsPath \ "h").format[Map[String, (Long, Long)]]
    )(Snapshot.apply, unlift(Snapshot.unapply))
}
