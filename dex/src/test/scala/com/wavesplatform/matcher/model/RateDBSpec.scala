package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.db.RateDB
import com.wavesplatform.{NoShrink, WithDB}
import org.scalacheck.Gen
import org.scalatest.{Matchers, WordSpecLike}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class RateDBSpec extends WordSpecLike with Matchers with WithDB with MatcherTestData with PropertyChecks with NoShrink {

  private def test(f: RateDB => Unit): Unit = f { RateDB(db) }

  "RateDB" should {
    "add, get and delete rates" in test { rdb =>
      val preconditions =
        Gen
          .listOfN(
            100,
            for {
              asset     <- arbitraryAssetIdGen
              rateValue <- Gen.choose(1, 100).map(_.toDouble / 100)
            } yield asset -> rateValue
          )
          .map(_.toMap)

      forAll(preconditions) { map =>
        map.foreach { case (asset, rateValue) => rdb.upsertRate(asset, rateValue) }
        rdb.getAllRates shouldBe map
        map.foreach { case (asset, _) => rdb.deleteRate(asset) }
        rdb.getAllRates.size shouldBe 0
      }
    }

    "update rate if it already exists" in test { rdb =>
      forAll(arbitraryAssetIdGen) { asset =>
        rdb.upsertRate(asset, 1)
        rdb.getAllRates shouldBe Map(asset -> 1)

        rdb.upsertRate(asset, 2)
        rdb.getAllRates shouldBe Map(asset -> 2)

        rdb.deleteRate(asset)
      }
    }
  }
}
