package com.wavesplatform.matcher

import com.google.common.base.Charsets.UTF_8
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.metrics._
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.AssetPair
import kamon.Kamon
import scorex.utils.ByteArray

class AssetPairBuilder(settings: MatcherSettings, blockchain: Blockchain) {
  import AssetPairBuilder._

  import Either.cond
  import Ordered._

  private[this] val indices             = settings.priceAssets.zipWithIndex.toMap
  private[this] val blacklistedAssetIds = settings.blacklistedAssets

  private[this] val timer    = Kamon.timer("matcher.asset-pair-builder")
  private[this] val create   = timer.refine("action" -> "create")
  private[this] val validate = timer.refine("action" -> "validate")

  private def isCorrectlyOrdered(pair: AssetPair): Boolean =
    (indices.get(pair.priceAssetStr), indices.get(pair.amountAssetStr)) match {
      case (None, None)         => pair.priceAsset < pair.amountAsset
      case (Some(_), None)      => true
      case (None, Some(_))      => false
      case (Some(pi), Some(ai)) => pi < ai
    }

  private def isNotBlacklisted(assetId: ByteStr): Boolean = blockchain.assetDescription(assetId).exists { d =>
    settings.blacklistedNames.forall(_.findFirstIn(new String(d.name, UTF_8)).isEmpty)
  }

  private def validateAssetId(assetId: Option[AssetId]): Either[String, Option[AssetId]] =
    cond(assetId.forall(isNotBlacklisted) && !blacklistedAssetIds(AssetPair.assetIdStr(assetId)), assetId, errorMsg(AssetPair.assetIdStr(assetId)))

  def validateAssetPair(pair: AssetPair): Either[String, AssetPair] =
    validate.measure(for {
      _ <- cond(pair.amountAsset != pair.priceAsset, (), "Amount and price assets must be different")
      _ <- cond(isCorrectlyOrdered(pair), pair, "Pair should be reverse")
      _ <- validateAssetId(pair.priceAsset)
      _ <- validateAssetId(pair.amountAsset)
    } yield pair)

  def createAssetPair(a1: String, a2: String): Either[String, AssetPair] =
    create.measure(for {
      a1 <- AssetPair.extractAssetId(a1).toEither.left.map(_ => errorMsg(a1))
      a2 <- AssetPair.extractAssetId(a2).toEither.left.map(_ => errorMsg(a2))
      p  <- validateAssetPair(AssetPair(a1, a2))
    } yield p)
}

object AssetPairBuilder {
  private def errorMsg(assetId: String) = s"Invalid Asset ID: $assetId"

  implicit val assetIdOrdering: Ordering[Option[ByteStr]] = (price: Option[ByteStr], amount: Option[ByteStr]) =>
    (price, amount) match {
      case (None, None)                           => 0
      case (_, None)                              => 1
      case (None, _)                              => -1
      case (Some(ByteStr(b1)), Some(ByteStr(b2))) => ByteArray.compare(b1, b2)
  }
}
