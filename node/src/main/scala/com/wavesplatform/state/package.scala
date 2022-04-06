package com.wavesplatform

import cats.{Id, Monad}
import cats.implicits._
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

  def sumMapF[F[_]: Monad, A, B](a: Map[A, B], b: Map[A, B], combine: (B, B) => F[B]): F[Map[A, B]] =
    a.foldLeft(b.pure[F]) {
      case (resultMapF, (key, next)) =>
        resultMapF.flatMap(
          resultMap =>
            if (resultMap.contains(key))
              combine(resultMap(key), next).map(r => resultMap + (key -> r))
            else
              (resultMap + (key -> next)).pure[F]
        )
    }

  implicit val safeSummarizer: Summarizer[Either[String, *]] = safeSum(_, _, _)
  implicit val unsafeSummarizer: Summarizer[Id]              = (x, y, _) => x + y

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
        case (addr, balance) => addr.toString -> balance
      })
  }

  object AssetDistributionPage extends TaggedType[Paged[Address, AssetDistribution]]
  type AssetDistributionPage = AssetDistributionPage.Type

  implicit val dstPageWrites: Writes[AssetDistributionPage] = Writes { page =>
    Json.obj(
      "hasNext"  -> JsBoolean(page.hasNext),
      "lastItem" -> Json.toJson(page.lastItem.map(_.toString)),
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
