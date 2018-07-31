package com.wavesplatform.matcher

import com.google.common.base.Charsets
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.settings.loadConfig
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.state.{AssetDescription, Blockchain, ByteStr}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FreeSpec, Matchers}

class AssetPairBuilderSpec extends FreeSpec with Matchers with MockFactory {
  import AssetPairBuilderSpec._

  private def b(v: String) = ByteStr.decodeBase58(v).get

  private val WAVES  = "WAVES"
  private val WUSD   = ByteStr.decodeBase58("HyFJ3rrq5m7FxdkWtQXkZrDat1F7LjVVGfpSkUuEXQHj").get
  private val WBTC   = ByteStr.decodeBase58("Fmg13HEHJHuZYbtJq8Da8wifJENq8uBxDuWoP9pVe2Qe").get
  private val WEUR   = ByteStr.decodeBase58("2xnE3EdpqXtFgCP156qt1AbyjpqdZ5jGjWo3CwTawcux").get
  private val WCNY   = ByteStr.decodeBase58("6pmDivReTLikwYqQtJTv6dTcE59knriaodB3AK8T9cF8").get
  private val Asset1 = mkAssetId(1)
  private val Asset2 = mkAssetId(2)
  private val Asset3 = mkAssetId(3)

  private val predefinedPriceAssets =
    Seq(
      WBTC,
      WUSD,
      WEUR,
      WCNY,
      b("8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"),
    )

  private val priceAssets = ConfigFactory.parseString(s"""waves.matcher {
       |  blacklisted-assets = [$Asset3]
       |  blacklisted-names = ["name$$"]
       |  price-assets = [${predefinedPriceAssets.mkString(",")}]
       |}""".stripMargin)
  private val settings    = MatcherSettings.fromConfig(loadConfig(priceAssets))
  private val blockchain  = stub[Blockchain]

  private val builder = new AssetPairBuilder(settings, blockchain)

  private val pairs = Table(
    ("amount", "price", "result"),
    (WAVES, WUSD.base58, Right(())),
    (WUSD.base58, WAVES, Left("Pair should be reverse")),
    (WBTC.base58, WEUR.base58, Left("Pair should be reverse")),
    (WEUR.base58, WBTC.base58, Right(())),
    (Asset1.base58, WAVES, Right(())),
    (WAVES, Asset1.base58, Left("Pair should be reverse")),
    (Asset2.base58, Asset1.base58, Right(())),
    (Asset1.base58, Asset2.base58, Left("Pair should be reverse")),
    (Asset1.base58, WBTC.base58, Right(())),
    (WEUR.base58, Asset1.base58, Left("Pair should be reverse")),
  )

  "AssetPairBuilder" - {
    "correctly orders pairs when assets IDs are valid" in {
      for (id <- predefinedPriceAssets) {
        (blockchain.assetDescription _).when(id).returns(mkAssetDescription())
      }

      (blockchain.assetDescription _).when(Asset1).returns(mkAssetDescription())
      (blockchain.assetDescription _).when(Asset2).returns(mkAssetDescription())

      forAll(pairs) {
        case (amountAsset, priceAsset, isValid) =>
          val pair = builder.createAssetPair(amountAsset, priceAsset)
          isValid match {
            case Right(_) => pair shouldBe 'right
            case Left(e)  => pair should produce(e)
          }
      }
    }
    "rejects a pair when" - {
      "blacklist" - {
        "contains asset id" in {
          (blockchain.assetDescription _).when(Asset3).returns(mkAssetDescription())
          builder.validateAssetPair(AssetPair(Some(Asset3), None)) should produce("Invalid Asset ID")
        }
        "matchers asset name" in {
          (blockchain.assetDescription _).when(Asset1).returns(mkAssetDescription())
          (blockchain.assetDescription _).when(Asset2).returns(mkAssetDescription("forbidden Asset name"))
          (blockchain.assetDescription _).when(Asset3).returns(mkAssetDescription("name of an asset"))

          builder.validateAssetPair(AssetPair(Some(Asset3), Some(Asset1))) should produce("Invalid Asset ID")
          builder.validateAssetPair(AssetPair(Some(Asset2), Some(Asset1))) should produce("Invalid Asset ID")
        }
      }
      "asset was not issued" in {}
      "amount and price assets are the same" in {
        builder.validateAssetPair(AssetPair(Some(WUSD), Some(WUSD))) should produce("Amount and price assets must be different")
      }
    }
  }
}

object AssetPairBuilderSpec {
  private def mkAssetId(index: Byte): ByteStr = ByteStr(Array.fill[Byte](32)(index))
  private def mkAssetDescription(assetName: String = ""): Option[AssetDescription] =
    Some(
      AssetDescription(PublicKeyAccount(Array.emptyByteArray),
                       assetName.getBytes(Charsets.UTF_8),
                       Array.emptyByteArray,
                       8,
                       false,
                       BigInt(1),
                       None,
                       0))
}
