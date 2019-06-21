package com.wavesplatform.matcher.db

import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.{NoShrink, WithDB}
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class AssetPairsDBSpec extends FreeSpec with Matchers with WithDB with MatcherTestData with PropertyChecks with NoShrink {

  private val fixedAssetPairGen = assetPairGen.filterNot(x => x.amountAsset == Waves && x.priceAsset == Waves)

  "Default AssetPairsDB implementation" - {
    "stores and reads all asset pairs" in {
      val g = Gen.containerOf[Set, AssetPair](fixedAssetPairGen)
      forAll(g) { assetPairs =>
        test { apdb =>
          assetPairs.foreach(apdb.add)
          apdb.all() shouldBe assetPairs
        }
      }
    }

    "removes asset pair" in {
      val g = for {
        xs <- Gen.nonEmptyContainerOf[Vector, AssetPair](fixedAssetPairGen)
        indexGen = Gen.choose(0, xs.size - 1)
        is <- Gen.nonEmptyListOf(indexGen)
      } yield (xs.toSet, is.toSet.map(xs.apply))

      forAll(g) {
        case (assetPairs, toRemove) =>
          test { apdb =>
            assetPairs.foreach(apdb.add)
            toRemove.foreach(apdb.remove)
            apdb.all() shouldBe (assetPairs -- toRemove)
          }
      }
    }

  }

  private def test(f: AssetPairsDB => Any): Any = tempDb(db => f(AssetPairsDB(db)))

}
