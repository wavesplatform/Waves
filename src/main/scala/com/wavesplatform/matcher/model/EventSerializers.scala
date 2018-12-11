package com.wavesplatform.matcher.model

import java.io.NotSerializableException
import java.nio.ByteBuffer

import akka.serialization._
import com.wavesplatform.matcher.market.MatcherActor.OrderBookCreated
import com.wavesplatform.matcher.market.OrderBookActor.Snapshot
import com.wavesplatform.matcher.market.{MatcherActor, OrderBookActor}
import com.wavesplatform.matcher.model.Events._
import com.wavesplatform.matcher.model.MatcherModel.{Level, Price}
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.transaction.assets.exchange._
import kamon.Kamon
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.collection.immutable.TreeMap

class EventSerializers extends SerializerWithStringManifest {
  import EventSerializers._

  private val serializationTimer = Kamon.timer("matcher.serialization.encode")
  private val bytesWritten       = Kamon.histogram("matcher.serialization.bytes-written")

  private def encodeAndMeasure[A: Writes](manifest: String, v: A): Array[Byte] =
    encodeAndMeasure(manifest, Json.toJson(v).toString().getBytes)

  private def encodeAndMeasure(manifest: String, f: => Array[Byte]): Array[Byte] = {
    val bytes = serializationTimer.refine("manifest" -> manifest).measure(f)
    bytesWritten.refine("manifest" -> manifest).record(bytes.length)
    bytes
  }

  override def identifier: Int = id
  override def manifest(o: AnyRef): String = o match {
    case _: OrderBookActor.Snapshot       => Manifest.Snapshot
    case _: MatcherActor.OrderBookCreated => Manifest.OrderBookCreated
    case _: OrderAdded                    => Manifest.OrderAddedV2
    case _: OrderExecuted                 => Manifest.OrderExecutedV2
    case _: OrderCanceled                 => Manifest.OrderCancelledV2
    case _: MatcherActor.Snapshot         => Manifest.MatcherSnapshot
  }

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case s: OrderBookActor.Snapshot         => encodeAndMeasure("order-book-snapshot", s)
    case obc: MatcherActor.OrderBookCreated => encodeAndMeasure("order-book-created", obc)
    case x: MatcherActor.Snapshot           => encodeAndMeasure("matcher-actor-snapshot", x)
    case oa: OrderAdded                     => encodeAndMeasure("order-added", encodeOrderAdded(oa))
    case oe: OrderExecuted                  => encodeAndMeasure("order-executed", encodeOrderExecuted(oe))
    case oc: OrderCanceled                  => encodeAndMeasure("order-cancelled", encodeOrderCancelled(oc))
  }

  override def fromBinary(bytes: Array[Byte], manifest: String): AnyRef = manifest match {
    case Manifest.Snapshot         => parse[Snapshot](bytes)
    case Manifest.OrderBookCreated => parse[OrderBookCreated](bytes)
    case Manifest.MatcherSnapshot  => parse[MatcherActor.Snapshot](bytes)
    case Manifest.OrderAddedV1     => parse[OrderAdded](bytes)
    case Manifest.OrderAddedV2     => decodeOrderAdded(bytes)
    case Manifest.OrderExecutedV1  => parse[OrderExecuted](bytes)
    case Manifest.OrderExecutedV2  => decodeOrderExecuted(bytes)
    case Manifest.OrderCancelledV1 => parse[OrderCanceled](bytes)
    case Manifest.OrderCancelledV2 => decodeOrderCancelled(bytes)
    case _                         => throw new NotSerializableException(manifest)
  }
}

object EventSerializers {
  private[EventSerializers] val id = 4001

  private[EventSerializers] object Manifest {
    val Snapshot         = "snapshot"
    val OrderBookCreated = "orderBookCreated"
    val MatcherSnapshot  = "matcherSnapshot"

    val OrderAddedV1 = "event.OrderAdded"
    val OrderAddedV2 = "event.OrderAdded.v2"

    val OrderExecutedV1 = "event.OrderExecuted"
    val OrderExecutedV2 = "event.OrderExecuted.v2"

    val OrderCancelledV1 = "event.OrderCancelled"
    val OrderCancelledV2 = "event.OrderCancelled.v2"
  }

  private def parse[A: Reads](bytes: Array[Byte]): A = Json.parse(bytes).as[A]

  private case class LO(amount: Long, fee: Option[Long], order: Order) {
    lazy val strictFee      = fee.getOrElse(LimitOrder.getPartialFee(order.matcherFee, order.amount, amount))
    def toLimit: LimitOrder = LimitOrder.limitOrder(amount, strictFee, order)
  }

  implicit val orderFormat: Format[Order]   = OrderJson.orderFormat
  private implicit val loFormat: Format[LO] = Json.format

  private def limitOrderFormatBuilder[T <: LimitOrder](f: LO => T): Format[T] = Format(
    js => JsSuccess(f(js.as[LO])),
    lo => Json.toJson(LO(lo.amount, Some(lo.fee), lo.order))
  )

  implicit val limitFormat: Format[LimitOrder]    = limitOrderFormatBuilder(_.toLimit)
  implicit val buyFormat: Format[BuyLimitOrder]   = limitOrderFormatBuilder(lo => BuyLimitOrder(lo.amount, lo.strictFee, lo.order))
  implicit val sellFormat: Format[SellLimitOrder] = limitOrderFormatBuilder(lo => SellLimitOrder(lo.amount, lo.strictFee, lo.order))

  implicit val orderMapWrites: Writes[Map[Price, Level[LimitOrder]]] = (tree: Map[Price, Level[LimitOrder]]) =>
    JsObject(tree.map {
      case (k, v) =>
        k.toString -> JsArray(v.map(o => Json.toJson(o)))
    })

  implicit val buyOrderTreeMapReads: Reads[TreeMap[Price, Level[BuyLimitOrder]]] = { jv: JsValue =>
    val a = jv.as[Map[String, Level[BuyLimitOrder]]].map { case (k, v) => (k.toLong, v) }
    JsSuccess(TreeMap.empty[Price, Level[BuyLimitOrder]](OrderBook.bidsOrdering) ++ a)
  }

  implicit val sellOrderTreeMapReads: Reads[TreeMap[Price, Level[SellLimitOrder]]] = { jv: JsValue =>
    val a = jv.as[Map[String, Level[SellLimitOrder]]].map { case (k, v) => (k.toLong, v) }
    JsSuccess(TreeMap.empty[Price, Level[SellLimitOrder]](OrderBook.asksOrdering) ++ a)
  }

  implicit val orderBookFormat: Format[OrderBook] = Json.format

  implicit val orderAddedReads: Reads[OrderAdded]        = js => JsSuccess(OrderAdded((js \ "o").as[LimitOrder]))
  implicit val orderExecutedFormat: Reads[OrderExecuted] = js => JsSuccess(OrderExecuted((js \ "o1").as[LimitOrder], (js \ "o2").as[LimitOrder]))
  implicit val orderCancelledFormat: Reads[OrderCanceled] = js =>
    JsSuccess(OrderCanceled((js \ "o").as[LimitOrder], (js \ "unmatchable").asOpt[Boolean].getOrElse(false)))

  private def mkOrderBookCreated(a1: String, a2: String) = OrderBookCreated(AssetPair.createAssetPair(a1, a2).get)
  private def orderBookToPair(obc: OrderBookCreated)     = (obc.pair.amountAssetStr, obc.pair.priceAssetStr)

  implicit val orderBookCreatedFormat: Format[OrderBookCreated] = ((__ \ "a1").format[String] and
    (__ \ "a2").format[String])(mkOrderBookCreated, orderBookToPair)

  implicit val assetPair: Format[AssetPair] = ((__ \ "a1").format[String] and
    (__ \ "a2").format[String])((a, b) => AssetPair.createAssetPair(a, b).get, x => (x.amountAssetStr, x.priceAssetStr))

  implicit val matcherSnapshot: Format[MatcherActor.Snapshot] = Json.format[MatcherActor.Snapshot]

  implicit val cacheFormat: Format[Map[String, (Price, Price)]] = new Format[Map[String, (Long, Long)]] {
    def writes(cache: Map[String, (Long, Long)]): JsValue =
      JsObject(cache.mapValues(v => Json.arr(v._1, v._2)))

    def reads(jv: JsValue): JsResult[Map[String, (Long, Long)]] =
      JsSuccess(jv.as[Map[String, (Long, Long)]])
  }

  implicit val snapshotFormat: Format[Snapshot] =
    Format((JsPath \ "o").read[OrderBook].map(Snapshot), Writes[Snapshot](s => Json.obj("o" -> s.orderBook)))

  private def encodeOrder(o: LimitOrder) = {
    val orderBytes = o.order.version match {
      case 1 => 1.toByte +: o.order.bytes()
      case 2 => o.order.bytes()
    }
    ByteBuffer
      .allocate(orderBytes.length + 16)
      .putLong(o.amount)
      .putLong(o.fee)
      .put(orderBytes)
      .array()
  }

  private def decodeOrder(bytes: Array[Byte]) = {
    val buf        = ByteBuffer.wrap(bytes)
    val amount     = buf.getLong
    val fee        = buf.getLong
    val orderBytes = bytes.drop(16)
    val order = orderBytes.head match {
      case 1 => OrderV1.parseBytes(orderBytes.tail).get
      case 2 => OrderV2.parseBytes(orderBytes).get
    }
    LimitOrder.limitOrder(amount, fee, order)
  }

  private def encodeOrderAdded(oa: OrderAdded)     = encodeOrder(oa.order)
  private def decodeOrderAdded(bytes: Array[Byte]) = OrderAdded(decodeOrder(bytes))

  private def encodeOrderExecuted(oe: OrderExecuted) = {
    val submittedBytes = encodeOrder(oe.submitted)
    val counterBytes   = encodeOrder(oe.counter)

    ByteBuffer
      .allocate(8 + submittedBytes.length + counterBytes.length)
      .putInt(submittedBytes.length)
      .put(submittedBytes)
      .putInt(counterBytes.length)
      .put(counterBytes)
      .array()
  }

  private def decodeOrderExecuted(bytes: Array[Byte]) = {
    val bb        = ByteBuffer.wrap(bytes)
    val submitted = new Array[Byte](bb.getInt)
    bb.get(submitted)
    val counter = new Array[Byte](bb.getInt)
    bb.get(counter)
    OrderExecuted(decodeOrder(submitted), decodeOrder(counter))
  }

  private def encodeOrderCancelled(oc: OrderCanceled)  = (if (oc.unmatchable) 1 else 0).toByte +: encodeOrder(oc.limitOrder)
  private def decodeOrderCancelled(bytes: Array[Byte]) = OrderCanceled(decodeOrder(bytes.tail), bytes.head == 1)
}
