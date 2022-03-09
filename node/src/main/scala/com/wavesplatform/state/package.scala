package com.wavesplatform

import cats.implicits.{catsSyntaxEitherId, toBifunctorOps}
import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.utils.Paged
import play.api.libs.json._
import supertagged.TaggedType

import scala.collection.immutable.Map
import scala.reflect.ClassTag
import scala.util.Try

package object state {
  def safeSum(x: Long, y: Long, source: String): Either[String, Long] =
    Try(Math.addExact(x, y)).toEither.leftMap(_ => s"$source sum overflow")

  def safeSumMap[A, B](a: Map[A, B], b: Map[A, B], combine: (B, B) => Either[String, B]): Either[String, Map[A, B]] =
    a.foldLeft(b.asRight[String]) {
      case (Right(resultMap), (key, next)) =>
        if (resultMap.contains(key))
          combine(resultMap(key), next).map(r => resultMap + (key -> r))
        else
          Right(resultMap + (key -> next))
      case (e @ Left(_), _) =>
        e
    }

  implicit class Cast[A](a: A) {
    def cast[B: ClassTag]: Option[B] = {
      a match {
        case b: B => Some(b)
        case _    => None
      }
    }
  }

  object AssetDistribution extends TaggedType[Map[Address, Long]]
  type AssetDistribution = AssetDistribution.Type

  implicit val dstMonoid: Monoid[AssetDistribution] = new Monoid[AssetDistribution] {
    override def empty: AssetDistribution = AssetDistribution(Map.empty[Address, Long])

    override def combine(x: AssetDistribution, y: AssetDistribution): AssetDistribution = {
      AssetDistribution(x ++ y)
    }
  }

  implicit val dstWrites: Writes[AssetDistribution] = Writes { dst =>
    Json
      .toJson(dst.map {
        case (addr, balance) => addr.stringRepr -> balance
      })
  }

  object AssetDistributionPage extends TaggedType[Paged[Address, AssetDistribution]]
  type AssetDistributionPage = AssetDistributionPage.Type

  implicit val dstPageWrites: Writes[AssetDistributionPage] = Writes { page =>
    Json.obj(
      "hasNext"  -> JsBoolean(page.hasNext),
      "lastItem" -> Json.toJson(page.lastItem.map(_.stringRepr)),
      "items"    -> Json.toJson(page.items)
    )
  }

  object Height extends TaggedType[Int]
  type Height = Height.Type

  object TxNum extends TaggedType[Short]
  type TxNum = TxNum.Type

  object TransactionId extends TaggedType[ByteStr]
  type TransactionId = TransactionId.Type
}
