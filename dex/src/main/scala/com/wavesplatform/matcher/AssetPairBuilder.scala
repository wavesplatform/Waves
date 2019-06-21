package com.wavesplatform.matcher

import cats.syntax.either._
import com.google.common.base.Charsets.UTF_8
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.AssetPairBuilder.AssetSide
import com.wavesplatform.matcher.error.MatcherError
import com.wavesplatform.matcher.settings.MatcherSettings
import com.wavesplatform.metrics._
import com.wavesplatform.state.{AssetDescription, Blockchain}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import kamon.Kamon

class AssetPairBuilder(settings: MatcherSettings, blockchain: Blockchain, blacklistedAssets: Set[IssuedAsset]) {
  import Either.cond
  import Ordered._

  type Result[T] = Either[MatcherError, T]

  private[this] val indices = settings.priceAssets.zipWithIndex.toMap

  private[this] val timer    = Kamon.timer("matcher.asset-pair-builder")
  private[this] val create   = timer.refine("action" -> "create")
  private[this] val validate = timer.refine("action" -> "validate")

  def isCorrectlyOrdered(pair: AssetPair): Boolean =
    (indices.get(pair.priceAssetStr), indices.get(pair.amountAssetStr)) match {
      case (None, None)         => pair.priceAsset.compatId < pair.amountAsset.compatId
      case (Some(_), None)      => true
      case (None, Some(_))      => false
      case (Some(pi), Some(ai)) => pi < ai
    }

  private def isBlacklistedByName(asset: IssuedAsset, desc: AssetDescription): Boolean =
    settings.blacklistedNames.exists(_.findFirstIn(new String(desc.name, UTF_8)).nonEmpty)

  def validateAssetId(asset: Asset): Result[Asset] = validateAssetId(asset, AssetSide.Unknown)

  private def validateAssetId(asset: Asset, side: AssetSide): Result[Asset] =
    asset.fold[Result[Asset]](Right(Waves)) { asset =>
      blockchain.assetDescription(asset).fold[Result[Asset]](Left(MatcherError.AssetNotFound(asset))) { desc =>
        if (blacklistedAssets.contains(asset) || isBlacklistedByName(asset, desc)) Left(side match {
          case AssetSide.Unknown => MatcherError.AssetBlacklisted(asset)
          case AssetSide.Amount => MatcherError.AmountAssetBlacklisted(asset)
          case AssetSide.Price => MatcherError.PriceAssetBlacklisted(asset)
        })
        else Right(asset)
      }
    }

  def validateAssetPair(pair: AssetPair): Result[AssetPair] =
    validate.measure {
      if (settings.allowedAssetPairs.contains(pair)) pair.asRight
      else if (settings.whiteListOnly) Left(MatcherError.AssetPairIsNotAllowed(pair))
      else
        for {
          _ <- cond(pair.amountAsset != pair.priceAsset, (), MatcherError.AssetPairSameAssets(pair.amountAsset))
          _ <- cond(isCorrectlyOrdered(pair), pair, MatcherError.AssetPairReversed(pair))
          _ <- validateAssetId(pair.priceAsset, AssetSide.Price)
          _ <- validateAssetId(pair.amountAsset, AssetSide.Amount)
        } yield pair
    }

  def createAssetPair(a1: String, a2: String): Result[AssetPair] =
    create.measure(for {
      a1 <- AssetPair.extractAssetId(a1).toEither.left.map(_ => MatcherError.InvalidAsset(a1))
      a2 <- AssetPair.extractAssetId(a2).toEither.left.map(_ => MatcherError.InvalidAsset(a2))
      p  <- validateAssetPair(AssetPair(a1, a2))
    } yield p)
}

object AssetPairBuilder {
  val assetIdOrdering: Ordering[Option[ByteStr]] = Ordering.Option[ByteStr]

  private sealed trait AssetSide
  private object AssetSide {
    case object Unknown extends AssetSide
    case object Amount  extends AssetSide
    case object Price   extends AssetSide
  }
}
