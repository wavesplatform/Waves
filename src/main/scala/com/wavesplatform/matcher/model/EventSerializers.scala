package com.wavesplatform.matcher.model

import akka.serialization._
import com.wavesplatform.matcher.market.OrderBookActor.Snapshot
import com.wavesplatform.matcher.model.Events._
import com.wavesplatform.matcher.model.MatcherModel.{Level, Price}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import scorex.crypto.encode.Base58
import scorex.transaction.AssetId
import scorex.transaction.assets.exchange.OrderJson._
import scorex.transaction.assets.exchange.{AssetPair, Order}

import scala.collection.immutable.TreeMap

class EventSerializer extends Serializer {
  import EventsJson._

  val includeManifest: Boolean = false
  val identifier = 1001

  def toBinary(obj: AnyRef): Array[Byte] = {
    obj match {
      case e: Event =>
        Json.stringify(EventFormat.writes(e)).getBytes
      case msg =>
        throw new Exception(s"Cannot serialize $msg with ${this.getClass}")
    }
  }

  def fromBinary(bytes: Array[Byte],
                 manifest: Option[Class[_]]): AnyRef = {
    val json = Json.parse(bytes)
    EventFormat.reads(json).get
  }
}

class SnapshotSerializer extends Serializer {
  import EventsJson._

  val includeManifest: Boolean = false
  val identifier = 2001

  def toBinary(obj: AnyRef): Array[Byte] = {
    obj match {
      case snap: Snapshot => Json.stringify(snapshotFormat.writes(snap)).getBytes
      case msg => throw new Exception(s"Cannot serialize $msg")
    }
  }

  def fromBinary(bytes: Array[Byte],
                 manifest: Option[Class[_]]): AnyRef = {
    val json = Json.parse(bytes)
    snapshotFormat.reads(json).get
  }
}

object EventsJson {

  implicit val limitOrderReads: Reads[LimitOrder] = (
    (JsPath \ "price").read[Long] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "order").read[Order]
    )(LimitOrder.limitOrder _)

  implicit val buyLimitOrderReads: Reads[BuyLimitOrder] = (
    (JsPath \ "price").read[Long] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "order").read[Order]
    )(BuyLimitOrder.apply _)

  implicit val sellLimOrderReads: Reads[SellLimitOrder] = (
    (JsPath \ "price").read[Long] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "order").read[Order]
    )(SellLimitOrder.apply _)

  implicit val limitOrderWrites = new Writes[LimitOrder] {
    def writes(o: LimitOrder): JsValue = Json.obj(
      "price" -> o.price,
      "amount" -> o.amount,
      "order" -> o.order.json
    )
  }

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

  implicit val orderBookWrites = new Writes[OrderBook] {
    def writes(o: OrderBook): JsValue = Json.obj(
      "bids" -> o.bids,
      "asks" -> o.asks
    )
  }

  implicit val orderBookReads: Reads[OrderBook] = (
    (JsPath \ "bids").read[TreeMap[Price, Level[BuyLimitOrder]]] and
      (JsPath \ "asks").read[TreeMap[Price, Level[SellLimitOrder]]]
    )(OrderBook.apply _)


  implicit val orderAddedWrites = new Writes[OrderAdded] {
    def writes(e: OrderAdded): JsValue = Json.obj("o" -> e.order)
  }

  implicit val orderAddedReads: Reads[OrderAdded] =
    (JsPath \ "o").read[LimitOrder](limitOrderReads).map(OrderAdded.apply)

  implicit val orderExecutedWrites = new Writes[OrderExecuted] {
    def writes(e: OrderExecuted): JsValue = Json.obj(
      "o1" -> e.submitted,
      "o2" -> e.counter
    )
  }

  implicit val orderExecutedReads: Reads[OrderExecuted] = (
    (JsPath \ "o1").read[LimitOrder](limitOrderReads) and
      (JsPath \ "o2").read[LimitOrder](limitOrderReads)
    )(OrderExecuted.apply _)

  implicit val orderCanceledWrites = new Writes[OrderCanceled] {
    def writes(e: OrderCanceled): JsValue = Json.obj("o" -> e.limitOrder)
  }

  implicit val orderCanceledReads: Reads[OrderCanceled] =
    (JsPath \ "o").read[LimitOrder](limitOrderReads).map(OrderCanceled.apply)

  implicit val orderBookeCreatedWrites = new Writes[OrderBookCreated] {
    def writes(e: OrderBookCreated): JsValue = Json.obj(
      "a1" -> e.pair.first.map(Base58.encode),
      "a2" -> e.pair.second.map(Base58.encode))
  }

  def readOrderBookCreated(a1: Option[Option[AssetId]], a2: Option[Option[AssetId]]) =
    OrderBookCreated(AssetPair(a1.flatten, a2.flatten))

  implicit val orderBookCreatedReads: Reads[OrderBookCreated] = {
    val r = (JsPath \ "a1").readNullable[Option[Array[Byte]]] and
      (JsPath \ "a2").readNullable[Option[Array[Byte]]]
    r.apply(readOrderBookCreated _)
  }

  object EventFormat extends Format[Event] {
    val OrderAddedId=  JsNumber(1)
    val OrderExecutedId =  JsNumber(2)
    val OrderCanceledId =  JsNumber(3)
    val OrderBookCreatedId =  JsNumber(4)

    override def writes(event: Event): JsValue = {
      event match {
        case e: OrderAdded =>
          Json.arr(OrderAddedId, orderAddedWrites.writes(e))
        case e: OrderExecuted =>
          Json.arr(OrderExecutedId, orderExecutedWrites.writes(e))
        case e: OrderCanceled =>
          Json.arr(OrderCanceledId, orderCanceledWrites.writes(e))
        case e: OrderBookCreated =>
          Json.arr(OrderBookCreatedId, orderBookeCreatedWrites.writes(e))
      }
    }

    override def reads(json: JsValue): JsResult[Event] = {
      json match {
        case JsArray(Seq(`OrderAddedId`, jsEvent)) =>
          orderAddedReads.reads(jsEvent)
        case JsArray(Seq(`OrderExecutedId`, jsEvent)) =>
          orderExecutedReads.reads(jsEvent)
        case JsArray(Seq(`OrderCanceledId`, jsEvent)) =>
          orderCanceledReads.reads(jsEvent)
        case JsArray(Seq(`OrderBookCreatedId`, jsEvent)) =>
          orderBookCreatedReads.reads(jsEvent)
        case e => JsError("Unexpected event:" + e)

      }
    }
  }

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

