package com.wavesplatform.state

import cats._
import cats.kernel.instances.map._
import com.wavesplatform.account.Address
import com.wavesplatform.block.Block.Fraction
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset._
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.collection.Seq
import scala.collection.immutable.Map

case class Portfolio(balance: Long, lease: LeaseBalance, assets: Map[IssuedAsset, Long]) {
  lazy val effectiveBalance: Long = safeSum(balance, lease.in) - lease.out
  lazy val spendableBalance: Long = balance - lease.out

  lazy val isEmpty: Boolean = this == Portfolio.empty

  def balanceOf(assetId: Asset): Long = assetId match {
    case Waves                  => balance
    case asset @ IssuedAsset(_) => assets.getOrElse(asset, 0L)
  }
}

object Portfolio {
  val empty: Portfolio = Portfolio(0L, Monoid[LeaseBalance].empty, Map.empty)

  def build(a: Asset, amount: Long): Portfolio = a match {
    case Waves              => Portfolio(amount, LeaseBalance.empty, Map.empty)
    case t @ IssuedAsset(_) => Portfolio(0L, LeaseBalance.empty, Map(t -> amount))
  }

  def combineAll(pfs: (Address, Portfolio)*): Map[Address, Portfolio] =
    Monoid.combineAll(pfs.map { case (addr, pf) => Map(addr -> pf) })

  def combineAllWaves(pfs: (Address, Long)*): Map[Address, Portfolio] =
    Monoid.combineAll(pfs.map { case (addr, balance) => Map(addr -> Portfolio(balance, LeaseBalance.empty, Map.empty)) })

  def combineAllAsset(asset: IssuedAsset)(pfs: (Address, Long)*): Map[Address, Portfolio] =
    Monoid.combineAll(pfs.map { case (addr, balance) => Map(addr -> Portfolio(0, LeaseBalance.empty, Map(asset -> balance))) })

  def combineAllWavesOrAsset(asset: Asset)(pfs: (Address, Long)*): Map[Address, Portfolio] = asset match {
    case ia: IssuedAsset => combineAllAsset(ia)(pfs: _*)
    case Asset.Waves     => combineAllWaves(pfs: _*)
  }

  implicit val safeLongSemigroup: Semigroup[Long] = (x: Long, y: Long) => safeSum(x, y)

  implicit val monoid: Monoid[Portfolio] = new Monoid[Portfolio] {
    override val empty: Portfolio = Portfolio.empty

    override def combine(older: Portfolio, newer: Portfolio): Portfolio =
      Portfolio(
        balance = safeSum(older.balance, newer.balance),
        lease = Monoid.combine(older.lease, newer.lease),
        assets = Monoid.combine(older.assets, newer.assets)
      )
  }

  implicit class PortfolioExt(self: Portfolio) {
    def spendableBalanceOf(assetId: Asset): Long = assetId.fold(self.spendableBalance)(self.assets.getOrElse(_, 0L))

    def pessimistic: Portfolio = Portfolio(
      balance = Math.min(self.balance, 0),
      lease = LeaseBalance(
        in = 0,
        out = Math.max(self.lease.out, 0)
      ),
      assets = self.assets.filter { case (_, v) => v < 0 }
    )

    def multiply(f: Fraction): Portfolio =
      Portfolio(f(self.balance), LeaseBalance.empty, self.assets.mapValues(f.apply))

    def minus(other: Portfolio): Portfolio =
      Portfolio(self.balance - other.balance, LeaseBalance.empty, Monoid.combine(self.assets, other.assets.mapValues(-_)))

    def negate: Portfolio = Portfolio.empty minus self

    def assetIds: Set[Asset] = {
      self.assets.keySet ++ Set(Waves)
    }

    def changedAssetIds(that: Portfolio): Set[Asset] = {
      val a1 = assetIds
      val a2 = that.assetIds

      val intersection = a1 & a2
      val sureChanged  = (a1 | a2) -- intersection
      intersection.filter(x => spendableBalanceOf(x) != that.spendableBalanceOf(x)) ++ sureChanged
    }
  }

  implicit val assetMapReads: Reads[Map[IssuedAsset, Long]] = Reads {
    case JsObject(fields) => {
      val keyReads = implicitly[Reads[Long]]
      val valueReads: String => JsResult[IssuedAsset] = (s: String) =>
        Base58
          .tryDecodeWithLimit(s)
          .fold(
            _ => JsError("Expected base58-encoded string"),
            arr => JsSuccess(IssuedAsset(ByteStr(arr)))
          )

      type Errors = Seq[(JsPath, Seq[JsonValidationError])]
      def locate(e: Errors, key: String) = e.map {
        case (p, valerr) => (JsPath \ key) ++ p -> valerr
      }

      fields
        .foldLeft(Right(Map.empty): Either[Errors, Map[IssuedAsset, Long]]) {
          case (acc, (key, value)) =>
            val result = for {
              rv <- keyReads.reads(value)
              rk <- valueReads(key)
            } yield rk -> rv

            (acc, result) match {
              case (Right(vs), JsSuccess(v, _)) => Right(vs + v)
              case (Right(_), JsError(e))       => Left(locate(e, key))
              case (Left(e), _: JsSuccess[_])   => Left(e)
              case (Left(e1), JsError(e2))      => Left(e1 ++ locate(e2, key))
            }
        }
        .fold(JsError.apply, res => JsSuccess(res))
    }

    case _ => JsError("error.expected.jsobject")
  }

  implicit val assetMapWrites: Writes[Map[IssuedAsset, Long]] = Writes { m =>
    Json.toJson(m.map {
      case (asset, balance) => asset.id.base58 -> JsNumber(balance)
    })
  }

  implicit val portfolioJsonReads: Reads[Portfolio] = (
    (JsPath \ "balance").read[Long] and
      (JsPath \ "lease").read[LeaseBalance] and
      (JsPath \ "assets").read[Map[IssuedAsset, Long]]
  )(Portfolio.apply _)

  implicit val portfolioJsonWrites: Writes[Portfolio] = Writes { pf =>
    JsObject(
      Map(
        "balance" -> JsNumber(pf.balance),
        "lease"   -> Json.toJson(pf.lease),
        "assets"  -> Json.toJson(pf.assets)
      )
    )
  }

  implicit val portfolioJsonFormat: Format[Portfolio] = Format(portfolioJsonReads, portfolioJsonWrites)
}
