package com.wavesplatform.matcher.model

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.v1.compiler.Terms.{FALSE, TRUE}
import com.wavesplatform.matcher.error._
import com.wavesplatform.matcher.market.OrderBookActor.MarketStatus
import com.wavesplatform.matcher.smart.MatcherScriptRunner
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.settings.DeviationsSettings
import com.wavesplatform.settings.fee.AssetType
import com.wavesplatform.settings.fee.OrderFeeSettings._
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.OrderOps._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.Verifier
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.utils.Time
import kamon.Kamon
import shapeless.Coproduct

import scala.Either.cond
import scala.math.BigDecimal.RoundingMode
import scala.util.control.NonFatal

object OrderValidator {

  type Result[T] = Either[MatcherError, T]

  private val timer = Kamon.timer("matcher.validation").refine("type" -> "blockchain")

  val MinExpiration: Long   = 60 * 1000L
  val MaxActiveOrders: Long = 200

  private def verifySignature(order: Order): Result[Order] =
    Verifier.verifyAsEllipticCurveSignature(order).leftMap(x => MatcherError.OrderInvalidSignature(order.id(), x.toString))

  private def verifyOrderByAccountScript(blockchain: Blockchain, address: Address, order: Order): Result[Order] =
    blockchain.accountScript(address).fold(verifySignature(order)) { script =>
      if (!blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height))
        MatcherError.ScriptedAccountTradingUnsupported.asLeft
      else if (order.version <= 1) MatcherError.OrderVersionUnsupportedWithScriptedAccount(address).asLeft
      else
        try MatcherScriptRunner(script, order, isTokenScript = false) match {
          case (_, Left(execError)) => MatcherError.AccountScriptReturnedError(address, execError).asLeft
          case (_, Right(FALSE))    => MatcherError.AccountScriptDeniedOrder(address).asLeft
          case (_, Right(TRUE))     => lift(order)
          case (_, Right(x))        => MatcherError.AccountScriptUnexpectResult(address, x.toString).asLeft
        } catch {
          case NonFatal(e) => MatcherError.AccountScriptException(address, e.getClass.getCanonicalName, e.getMessage).asLeft
        }
    }

  private def verifySmartToken(blockchain: Blockchain, asset: IssuedAsset, tx: ExchangeTransaction): Result[Unit] =
    blockchain.assetScript(asset).fold(success) { script =>
      if (!blockchain.isFeatureActivated(BlockchainFeatures.SmartAssets, blockchain.height))
        MatcherError.ScriptedAssetTradingUnsupported(asset).asLeft
      else
        try ScriptRunner(blockchain.height, Coproduct(tx), blockchain, script, isTokenScript = true) match {
          case (_, Left(execError)) => MatcherError.AssetScriptReturnedError(asset, execError).asLeft
          case (_, Right(FALSE))    => MatcherError.AssetScriptDeniedOrder(asset).asLeft
          case (_, Right(TRUE))     => success
          case (_, Right(x))        => MatcherError.AssetScriptUnexpectResult(asset, x.toString).asLeft
        } catch {
          case NonFatal(e) => MatcherError.AssetScriptException(asset, e.getClass.getCanonicalName, e.getMessage).asLeft
        }
    }

  private def decimals(blockchain: Blockchain, assetId: Asset): Result[Int] =
    assetId.fold(lift(8)) { aid =>
      blockchain.assetDescription(aid).map(_.decimals).toRight(MatcherError.AssetNotFound(aid))
    }

  private def validateDecimals(blockchain: Blockchain, o: Order): Result[Unit] =
    for {
      pd <- decimals(blockchain, o.assetPair.priceAsset)
      ad <- decimals(blockchain, o.assetPair.amountAsset)
      insignificantDecimals = (pd - ad).max(0)
      _ <- cond(
        o.price % BigDecimal(10).pow(insignificantDecimals).toLongExact == 0,
        (),
        MatcherError.PriceLastDecimalsMustBeZero(insignificantDecimals)
      )
    } yield ()

  def blockchainAware(
      blockchain: Blockchain,
      transactionCreator: (LimitOrder, LimitOrder, Long) => Either[ValidationError, ExchangeTransaction],
      matcherAddress: Address,
      time: Time,
      orderFeeSettings: OrderFeeSettings
  )(order: Order): Result[Order] = timer.measure {
    lazy val exchangeTx: Result[ExchangeTransaction] = {
      val fakeOrder: Order = order.updateType(order.orderType.opposite)
      transactionCreator(LimitOrder(fakeOrder), LimitOrder(order), time.correctedTime()).left.map { x =>
        MatcherError.CanNotCreateExchangeTransaction(x.toString)
      }
    }

    def verifyAssetScript(assetId: Asset): Result[Unit] = assetId.fold(success) { assetId =>
      exchangeTx.flatMap(verifySmartToken(blockchain, assetId, _))
    }

    lazy val mof = ExchangeTransactionCreator.minFee(blockchain, matcherAddress, order.assetPair)
    for {
      _ <- lift(order)
        .ensure(MatcherError.OrderVersionUnsupported(order.version, BlockchainFeatures.SmartAccountTrading)) {
          _.version == 1 || blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height)
        }
        .ensure(MatcherError.OrderVersionUnsupported(order.version, BlockchainFeatures.OrderV3)) {
          _.version != 3 || blockchain.isFeatureActivated(BlockchainFeatures.OrderV3, blockchain.height)
        }
        .ensure(MatcherError.FeeNotEnough(mof, order.matcherFee, Waves)) { o =>
          orderFeeSettings match {
            case _: FixedWavesSettings => o.matcherFee >= mof
            case _                     => true
          }
        }
      _ <- validateDecimals(blockchain, order)
      _ <- verifyOrderByAccountScript(blockchain, order.sender, order)
      _ <- verifyAssetScript(order.assetPair.amountAsset)
      _ <- verifyAssetScript(order.assetPair.priceAsset)
    } yield order
  }

  private def validateBalance(order: Order, tradableBalance: Asset => Long): Result[Order] = {
    val lo               = LimitOrder(order)
    val requiredForOrder = lo.requiredBalance

    val available = requiredForOrder.keySet.map { assetId =>
      assetId -> tradableBalance(assetId)
    }.toMap

    val negativeBalances = Monoid.combine(available, requiredForOrder.mapValues(-_)).filter(_._2 < 0)
    cond(negativeBalances.isEmpty, order, MatcherError.BalanceNotEnough(requiredForOrder, available))
  }

  private[matcher] def getValidFeeAssetForSettings(order: Order, orderFeeSettings: OrderFeeSettings): Asset = orderFeeSettings match {
    case _: FixedWavesSettings            => Waves
    case FixedSettings(defaultAssetId, _) => defaultAssetId
    case PercentSettings(assetType, _) =>
      assetType match {
        case AssetType.AMOUNT    => order.assetPair.amountAsset
        case AssetType.PRICE     => order.assetPair.priceAsset
        case AssetType.RECEIVING => order.getReceiveAssetId
        case AssetType.SPENDING  => order.getSpendAssetId
      }
  }

  private[matcher] def getMinValidFeeForSettings(order: Order, orderFeeSettings: OrderFeeSettings, matchPrice: Long, multiplier: Double = 1): Long = {

    def multiplyLongByDoubleRoundUp(l: Long, d: Double): Long = (BigDecimal(l) * d).setScale(0, RoundingMode.HALF_UP).toLong

    orderFeeSettings match {
      case FixedWavesSettings(wavesMinFee) => wavesMinFee
      case FixedSettings(_, fixedMinFee)   => fixedMinFee
      case PercentSettings(assetType, minFeeInPercent) =>
        lazy val receiveAmount = order.getReceiveAmount(order.amount, matchPrice).explicitGet()
        lazy val spentAmount   = order.getSpendAmount(order.amount, matchPrice).explicitGet()

        val amountFactor = assetType match {
          case AssetType.AMOUNT    => order.amount
          case AssetType.PRICE     => if (order.orderType == OrderType.BUY) spentAmount else receiveAmount
          case AssetType.RECEIVING => receiveAmount
          case AssetType.SPENDING  => spentAmount
        }

        multiplyLongByDoubleRoundUp(amountFactor, multiplier * minFeeInPercent / 100)
    }
  }

  def validateOrderFeeWithDeviations(order: Order,
                                     orderFeeSettings: OrderFeeSettings,
                                     deviationSettings: DeviationsSettings,
                                     marketStatus: Option[MarketStatus]): Result[Order] = {
    if (order.version < 3) lift(order)
    else {

      lazy val requiredFeeAsset: Asset = getValidFeeAssetForSettings(order, orderFeeSettings)
      lazy val requiredFee: Long       = getMinValidFeeForSettings(order, orderFeeSettings, order.price)

      lazy val validateFeeWithoutDeviation =
        lift(order).ensure(MatcherError.FeeNotEnough(requiredFee, order.matcherFee, requiredFeeAsset))(_.matcherFee >= requiredFee)

      lazy val validateFeeWithDeviation = orderFeeSettings match {
        case percentSettings: PercentSettings =>
          (for {
            ms      <- marketStatus
            bestBid <- ms.bestBid
            bestAsk <- ms.bestAsk
          } yield {

            val minValidFeeWithDeviation = {
              val matchedPrice = if (order.orderType == OrderType.BUY) bestAsk.price else bestBid.price
              getMinValidFeeForSettings(order, percentSettings, matchedPrice, 1 - (deviationSettings.maxPriceFee / 100))
            }

            Either.cond(order.matcherFee >= minValidFeeWithDeviation, order, MatcherError.DeviantOrderMatcherFee(order, deviationSettings))

          }) getOrElse validateFeeWithoutDeviation
        case _ => validateFeeWithoutDeviation
      }

      for {
        _ <- lift(order).ensure(MatcherError.UnexpectedFeeAsset(requiredFeeAsset, order.matcherFeeAssetId))(_.matcherFeeAssetId == requiredFeeAsset)
        _ <- validateFeeWithDeviation
      } yield order
    }
  }

  def validatePriceDeviation(order: Order, deviationSettings: DeviationsSettings, marketStatus: Option[MarketStatus]): Result[Order] = {

    def multiplyLongByDoubleRoundDown(l: Long, d: Double): Long = (BigDecimal(l) * d).setScale(0, RoundingMode.HALF_DOWN).toLong

    val validatePrice: (Double, Double) => Boolean = (subtractedPercent, addedPercent) => {

      val res = for {
        ms      <- marketStatus
        bestBid <- ms.bestBid
        bestAsk <- ms.bestAsk
      } yield {

        val lowerBound = multiplyLongByDoubleRoundDown(bestBid.price, 1 - (subtractedPercent / 100))
        val upperBound = multiplyLongByDoubleRoundDown(bestAsk.price, 1 + (addedPercent / 100))

        lowerBound <= order.price && order.price <= upperBound
      }

      res getOrElse true
    }

    lift(order).ensure(MatcherError.DeviantOrderPrice(order, deviationSettings)) { _ =>
      if (order.orderType == OrderType.BUY) validatePrice(deviationSettings.maxPriceProfit, deviationSettings.maxPriceLoss)
      else validatePrice(deviationSettings.maxPriceLoss, deviationSettings.maxPriceProfit)
    }
  }

  def matcherSettingsAware(
      matcherPublicKey: PublicKeyAccount,
      blacklistedAddresses: Set[Address],
      blacklistedAssets: Set[IssuedAsset],
      orderFeeSettings: OrderFeeSettings,
      deviationSettings: DeviationsSettings,
      getMarketStatus: AssetPair => Option[MarketStatus]
  )(order: Order): Result[Order] = {

    def validateBlacklistedAsset(assetId: Asset, e: IssuedAsset => MatcherError): Result[Unit] =
      assetId.fold(success)(x => cond(!blacklistedAssets(x), (), e(x)))

    for {
      _ <- lift(order)
        .ensure(MatcherError.UnexpectedMatcherPublicKey(matcherPublicKey, order.matcherPublicKey))(_.matcherPublicKey == matcherPublicKey)
        .ensure(MatcherError.AddressIsBlacklisted(order.sender))(o => !blacklistedAddresses.contains(o.sender.toAddress))
      _ <- validateBlacklistedAsset(order.assetPair.amountAsset, MatcherError.AmountAssetBlacklisted)
      _ <- validateBlacklistedAsset(order.assetPair.priceAsset, MatcherError.PriceAssetBlacklisted)
      _ <- validateBlacklistedAsset(order.matcherFeeAssetId, MatcherError.FeeAssetBlacklisted)
      _ <- validateOrderFeeWithDeviations(order, orderFeeSettings, deviationSettings, getMarketStatus(order.assetPair))
      _ <- validatePriceDeviation(order, deviationSettings, getMarketStatus(order.assetPair))
    } yield order
  }

  def timeAware(time: Time)(order: Order): Result[Order] = {
    for {
      _ <- cond(order.expiration > time.correctedTime() + MinExpiration,
                (),
                MatcherError.WrongExpiration(time.correctedTime(), MinExpiration, order.expiration))
      _ <- order.isValid(time.correctedTime()).toEither.left.map(MatcherError.OrderCommonValidationFailed)
    } yield order
  }

  def accountStateAware(
      sender: Address,
      tradableBalance: Asset => Long,
      activeOrderCount: => Int,
      orderExists: ByteStr => Boolean,
  )(order: Order): Result[Order] =
    for {
      _ <- lift(order)
        .ensure(MatcherError.UnexpectedSender(order.sender.toAddress, sender))(_.sender.toAddress == sender)
        .ensure(MatcherError.ActiveOrdersLimitReached(MaxActiveOrders))(_ => activeOrderCount < MaxActiveOrders)
        .ensure(MatcherError.OrderDuplicate(order.id()))(o => !orderExists(o.id()))
      _ <- validateBalance(order, tradableBalance)
    } yield order

  private def lift[T](x: T): Result[T] = x.asRight[MatcherError]
  private def success: Result[Unit]    = lift(())
}
