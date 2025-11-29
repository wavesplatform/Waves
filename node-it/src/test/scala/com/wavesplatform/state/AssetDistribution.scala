package com.wavesplatform.state

import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.utils.Paged
import play.api.libs.json.{JsBoolean, Json, Writes}

type AssetDistribution = Map[Address, Long]
object AssetDistribution {
  given Monoid[AssetDistribution] = new Monoid[AssetDistribution] {
    override def empty: AssetDistribution = Map.empty[Address, Long]

    override def combine(x: AssetDistribution, y: AssetDistribution): AssetDistribution = x ++ y
  }

  given Writes[AssetDistribution] = Writes { dst =>
    Json
      .toJson(dst.map { case (addr, balance) =>
        addr.toString -> balance
      })
  }

}

type AssetDistributionPage = Paged[Address, AssetDistribution]
object AssetDistributionPage {
  def apply(p: Paged[Address, AssetDistribution]): AssetDistributionPage = p
  given Writes[AssetDistributionPage] = Writes { page =>
    Json.obj(
      "hasNext"  -> JsBoolean(page.hasNext),
      "lastItem" -> Json.toJson(page.lastItem.map(_.toString)),
      "items"    -> Json.toJson(page.items)
    )
  }
}
