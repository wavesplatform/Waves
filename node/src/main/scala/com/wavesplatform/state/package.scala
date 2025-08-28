package com.wavesplatform

import cats.Id
import cats.implicits.*
import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.bls.BlsPublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.utils.Paged
import play.api.libs.json.*
import supertagged.TaggedType

import scala.reflect.ClassTag
import scala.util.Try

package object state {
  def safeSum(x: Long, y: Long, source: String): Either[String, Long] =
    Try(Math.addExact(x, y)).toEither.leftMap(_ => s"$source sum overflow")

  implicit val safeSummarizer: Summarizer[[X] =>> Either[String, X]] = safeSum(_, _, _)
  implicit val unsafeSummarizer: Summarizer[Id]                      = (x, y, _) => x + y

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
      .toJson(dst.map { case (addr, balance) =>
        addr.toString -> balance
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

  type GeneratorBalances = Map[(BlsPublicKey, Address), Long]

  object Height extends TaggedType[Int] {
    implicit val format: Format[Height] = implicitly[Format[Int]].bimap(Height(_), identity)
    implicit final class Ops(val self: Height) {
      def next: Height = Height(self + 1)
    }
  }
  type Height = Height.Type
  val GenesisBlockHeight = Height(1)

  object TxNum extends TaggedType[Short]
  type TxNum = TxNum.Type

  object AssetNum extends TaggedType[Int]
  type AssetNum = AssetNum.Type

  object TransactionId extends TaggedType[ByteStr] {
    implicit val format: Format[TransactionId] = Format[TransactionId](
      com.wavesplatform.utils.byteStrFormat.map(this(_)),
      Writes(com.wavesplatform.utils.byteStrFormat.writes)
    )
  }
  type TransactionId = TransactionId.Type

  type CompleteBlockchainUpdater = Blockchain & BlockchainUpdater & NG
}
