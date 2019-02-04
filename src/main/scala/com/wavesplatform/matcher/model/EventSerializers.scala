package com.wavesplatform.matcher.model

import java.io.NotSerializableException

import akka.serialization._
import com.wavesplatform.matcher.market.MatcherActor.OrderBookCreated
import com.wavesplatform.matcher.market.OrderBookActor.Snapshot
import com.wavesplatform.matcher.market.{MatcherActor, OrderBookActor}
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.transaction.assets.exchange._
import kamon.Kamon
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

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
    case _: MatcherActor.Snapshot         => Manifest.MatcherSnapshot
  }

  override def toBinary(o: AnyRef): Array[Byte] = o match {
    case s: OrderBookActor.Snapshot         => encodeAndMeasure("order-book-snapshot", s)
    case obc: MatcherActor.OrderBookCreated => encodeAndMeasure("order-book-created", obc)
    case x: MatcherActor.Snapshot           => encodeAndMeasure("matcher-actor-snapshot", x)
  }

  override def fromBinary(bytes: Array[Byte], manifest: String): AnyRef = manifest match {
    case Manifest.Snapshot         => parse[Snapshot](bytes)
    case Manifest.OrderBookCreated => parse[OrderBookCreated](bytes)
    case Manifest.MatcherSnapshot  => parse[MatcherActor.Snapshot](bytes)
    case _                         => throw new NotSerializableException(manifest)
  }
}

object EventSerializers {
  private[EventSerializers] val id = 4001

  private[EventSerializers] object Manifest {
    val Snapshot         = "snapshot"
    val OrderBookCreated = "orderBookCreated"
    val MatcherSnapshot  = "matcherSnapshot"
  }

  private def parse[A: Reads](bytes: Array[Byte]): A = Json.parse(bytes).as[A]

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

  implicit val snapshotFormat: Format[Snapshot] = Format(
    ((JsPath \ "n").readNullable[Long].map(_.getOrElse(-1L)) and (JsPath \ "o").read[OrderBook.Snapshot])(Snapshot),
    Writes[Snapshot](s => Json.obj("n" -> s.eventNr, "o" -> s.orderBook))
  )
}
