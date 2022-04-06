package com.wavesplatform.state.diffs

import cats.implicits.toFoldableOps
import cats.syntax.either.*
import com.wavesplatform.account.Address
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{GenericError, OrderValidationError}
import com.wavesplatform.transaction.assets.exchange.OrderPriceMode.AssetDecimals
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order, OrderPriceMode, OrderType}
import com.wavesplatform.transaction.{Asset, TxVersion}

import scala.util.{Right, Try}

object ExchangeTransactionDiff {

  def apply(blockchain: Blockchain)(tx: ExchangeTransaction): Either[ValidationError, Diff] = {
    val buyer  = tx.buyOrder.senderAddress
    val seller = tx.sellOrder.senderAddress

    val assetIds =
      List(
        tx.buyOrder.assetPair.amountAsset,
        tx.buyOrder.assetPair.priceAsset,
        tx.sellOrder.assetPair.amountAsset,
        tx.sellOrder.assetPair.priceAsset
      ).collect { case asset: IssuedAsset =>
        asset
      }.distinct
    val assets = assetIds.map(id => id -> blockchain.assetDescription(id)).toMap

    def smartFeaturesChecks(): Either[GenericError, (Int, Boolean, Boolean)] =
      for {
        _ <- Right(())
        smartTradesEnabled = blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading)
        smartAssetsEnabled = blockchain.isFeatureActivated(BlockchainFeatures.SmartAssets)
        assetsScripted     = assets.values.count(_.flatMap(_.script).isDefined)
        _ <- Either.cond(
          smartAssetsEnabled || assetsScripted == 0,
          (),
          GenericError(s"Smart assets can't participate in ExchangeTransactions (SmartAssetsFeature is disabled)")
        )
        buyerScripted = blockchain.hasAccountScript(buyer)
        _ <- Either.cond(
          smartTradesEnabled || !buyerScripted,
          (),
          GenericError(s"Buyer $buyer can't participate in ExchangeTransaction because it has assigned Script (SmartAccountsTrades is disabled)")
        )
        sellerScripted = blockchain.hasAccountScript(seller)
        _ <- Either.cond(
          smartTradesEnabled || !sellerScripted,
          (),
          GenericError(s"Seller $seller can't participate in ExchangeTransaction because it has assigned Script (SmartAccountsTrades is disabled)")
        )
      } yield (assetsScripted, buyerScripted, sellerScripted)

    for {
      buyerAndSellerScripted <- smartFeaturesChecks()
      portfolios             <- getPortfolios(blockchain, tx)
      _                      <- enoughVolume(tx, blockchain)
      _                      <- checkOrderPriceModes(tx, blockchain)
      scripts = {
        val (assetsScripted, buyerScripted, sellerScripted) = buyerAndSellerScripted
        val matcherScripted                                 = Some(tx.sender.toAddress).count(blockchain.hasAccountScript)

        // Don't count before Ride4DApps activation
        val ordersScripted = Seq(buyerScripted, sellerScripted)
          .filter(_ => blockchain.isFeatureActivated(BlockchainFeatures.Ride4DApps, blockchain.height))
          .count(identity)

        assetsScripted +
          matcherScripted +
          ordersScripted
      }
    } yield {
      Diff(
        portfolios = portfolios,
        orderFills = Map(
          tx.buyOrder.id()  -> VolumeAndFee(tx.amount.value, tx.buyMatcherFee),
          tx.sellOrder.id() -> VolumeAndFee(tx.amount.value, tx.sellMatcherFee)
        ),
        scriptsRun = scripts
      )
    }
  }

  def getPortfolios(blockchain: Blockchain, tx: ExchangeTransaction): Either[ValidationError, Map[Address, Portfolio]] = {
    def isPriceValid(amountDecimals: Int, priceDecimals: Int) = {
      def convertPrice(price: Long, amountDecimals: Int, priceDecimals: Int) =
        Try {
          (BigDecimal(price) / BigDecimal(10).pow(priceDecimals - amountDecimals)).toBigInt.bigInteger.longValueExact()
        }.toEither.leftMap(x => GenericError(x.getMessage))

      def orderPrice(order: Order, amountDecimals: Int, priceDecimals: Int) =
        if (tx.version >= TxVersion.V3 && (order.version < Order.V4 || order.priceMode == AssetDecimals))
          convertPrice(order.price.value, amountDecimals, priceDecimals)
        else
          Right(order.price.value)

      for {
        buyOrderPrice  <- orderPrice(tx.buyOrder, amountDecimals, priceDecimals)
        sellOrderPrice <- orderPrice(tx.sellOrder, amountDecimals, priceDecimals)
        _              <- Either.cond(tx.price.value <= buyOrderPrice, (), GenericError("price should be <= buyOrder.price"))
        _              <- Either.cond(tx.price.value >= sellOrderPrice, (), GenericError("price should be >= sellOrder.price"))
      } yield ()
    }

    val assetIds =
      List(
        tx.buyOrder.assetPair.amountAsset,
        tx.buyOrder.assetPair.priceAsset,
        tx.sellOrder.assetPair.amountAsset,
        tx.sellOrder.assetPair.priceAsset
      ).collect { case asset: IssuedAsset =>
        asset
      }.distinct
    val assets = assetIds.map(id => id -> blockchain.assetDescription(id)).toMap

    val matcher: Address = tx.sender.toAddress
    val buyer: Address   = tx.buyOrder.sender.toAddress
    val seller: Address  = tx.sellOrder.sender.toAddress

    def getAssetDiff(asset: Asset, buyAssetChange: Long, sellAssetChange: Long): Either[String, Map[Address, Portfolio]] = {
      Diff.combine(
        Map(buyer  -> Portfolio.build(asset, buyAssetChange)),
        Map(seller -> Portfolio.build(asset, sellAssetChange))
      )
    }

    lazy val matcherPortfolioE =
      Seq(
        getOrderFeePortfolio(tx.buyOrder, tx.buyMatcherFee),
        getOrderFeePortfolio(tx.sellOrder, tx.sellMatcherFee),
        Portfolio.waves(-tx.fee.value)
      ).foldM(Portfolio())(_.combine(_))

    lazy val feeDiffE =
      matcherPortfolioE.flatMap(
        matcherPortfolio =>
          Seq(
            Map[Address, Portfolio](matcher -> matcherPortfolio),
            Map[Address, Portfolio](buyer   -> getOrderFeePortfolio(tx.buyOrder, -tx.buyMatcherFee)),
            Map[Address, Portfolio](seller  -> getOrderFeePortfolio(tx.sellOrder, -tx.sellMatcherFee))
          ).foldM(Map.empty[Address, Portfolio])(Diff.combine)
      )

    for {
      _ <- Either.cond(
        tx.buyMatcherFee >= 0 && tx.sellMatcherFee >= 0,
        (),
        GenericError("Matcher fee can not be negative")
      )
      _ <- Either.cond(assets.values.forall(_.isDefined), (), GenericError("Assets should be issued before they can be traded"))
      amountDecimals = if (tx.version < TxVersion.V3) 8 else tx.buyOrder.assetPair.amountAsset.fold(8)(ia => assets(ia).fold(8)(_.decimals))
      priceDecimals  = if (tx.version < TxVersion.V3) 8 else tx.buyOrder.assetPair.priceAsset.fold(8)(ia => assets(ia).fold(8)(_.decimals))
      _                     <- isPriceValid(amountDecimals, priceDecimals)
      buyPriceAssetChange   <- getSpendAmount(tx.buyOrder, amountDecimals, priceDecimals, tx.amount.value, tx.price.value).map(-_)
      buyAmountAssetChange  <- getReceiveAmount(tx.buyOrder, amountDecimals, priceDecimals, tx.amount.value, tx.price.value)
      sellPriceAssetChange  <- getReceiveAmount(tx.sellOrder, amountDecimals, priceDecimals, tx.amount.value, tx.price.value)
      sellAmountAssetChange <- getSpendAmount(tx.sellOrder, amountDecimals, priceDecimals, tx.amount.value, tx.price.value).map(-_)
      priceDiff             <- getAssetDiff(tx.buyOrder.assetPair.priceAsset, buyPriceAssetChange, sellPriceAssetChange).leftMap(GenericError(_))
      amountDiff            <- getAssetDiff(tx.buyOrder.assetPair.amountAsset, buyAmountAssetChange, sellAmountAssetChange).leftMap(GenericError(_))
      feeDiff               <- feeDiffE.leftMap(GenericError(_))
      totalDiff             <- Diff.combine(feeDiff, priceDiff).flatMap(Diff.combine(_, amountDiff)).leftMap(GenericError(_))
    } yield totalDiff
  }

  private[this] def checkOrderPriceModes(tx: ExchangeTransaction, blockchain: Blockchain): Either[GenericError, Unit] = {
    def isLegacyModeOrder(order: Order) = order.version >= Order.V4 && order.priceMode != OrderPriceMode.Default
    Either.cond(
      !Seq(tx.order1, tx.order2).exists(isLegacyModeOrder) || blockchain.isFeatureActivated(BlockchainFeatures.RideV6),
      (),
      GenericError("Legacy price mode is only available after RideV6 activation")
    )
  }

  private def enoughVolume(exTrans: ExchangeTransaction, blockchain: Blockchain): Either[ValidationError, Unit] = {

    val filledBuy  = blockchain.filledVolumeAndFee(exTrans.buyOrder.id())
    val filledSell = blockchain.filledVolumeAndFee(exTrans.sellOrder.id())

    val buyTotal  = filledBuy.volume + exTrans.amount.value
    val sellTotal = filledSell.volume + exTrans.amount.value

    lazy val buyAmountValid  = exTrans.buyOrder.amount.value >= buyTotal
    lazy val sellAmountValid = exTrans.sellOrder.amount.value >= sellTotal

    def isFeeValid(feeTotal: Long, amountTotal: Long, maxfee: Long, maxAmount: Long, order: Order): Boolean = {
      feeTotal <= (order match {
        case o: Order if o.version >= Order.V3 => BigInt(maxfee)
        case _                                 => BigInt(maxfee) * BigInt(amountTotal) / BigInt(maxAmount)
      })
    }

    lazy val buyFeeValid =
      isFeeValid(
        feeTotal = filledBuy.fee + exTrans.buyMatcherFee,
        amountTotal = buyTotal,
        maxfee = exTrans.buyOrder.matcherFee.value,
        maxAmount = exTrans.buyOrder.amount.value,
        order = exTrans.buyOrder
      )

    lazy val sellFeeValid =
      isFeeValid(
        feeTotal = filledSell.fee + exTrans.sellMatcherFee,
        amountTotal = sellTotal,
        maxfee = exTrans.sellOrder.matcherFee.value,
        maxAmount = exTrans.sellOrder.amount.value,
        order = exTrans.sellOrder
      )

    if (!buyAmountValid) Left(OrderValidationError(exTrans.buyOrder, s"Too much buy. Already filled volume for the order: ${filledBuy.volume}"))
    else if (!sellAmountValid)
      Left(OrderValidationError(exTrans.sellOrder, s"Too much sell. Already filled volume for the order: ${filledSell.volume}"))
    else if (!buyFeeValid) Left(OrderValidationError(exTrans.buyOrder, s"Insufficient buy fee"))
    else if (!sellFeeValid) Left(OrderValidationError(exTrans.sellOrder, s"Insufficient sell fee"))
    else Right(())
  }

  private[diffs] def getSpendAmount(
      order: Order,
      amountDecimals: Int,
      priceDecimals: Int,
      matchAmount: Long,
      matchPrice: Long
  ): Either[ValidationError, Long] =
    Try {
      if (order.orderType == OrderType.SELL) matchAmount
      else {
        val spend = (BigDecimal(matchAmount) * matchPrice * BigDecimal(10).pow(priceDecimals - amountDecimals - 8)).toBigInt
        if (order.getSpendAssetId == Waves && !(spend + order.matcherFee.value).isValidLong) {
          throw new ArithmeticException("BigInteger out of long range")
        } else spend.bigInteger.longValueExact()
      }
    }.toEither.left.map(x => GenericError(x.getMessage))

  private[diffs] def getReceiveAmount(
      order: Order,
      amountDecimals: Int,
      priceDecimals: Int,
      matchAmount: Long,
      matchPrice: Long
  ): Either[ValidationError, Long] =
    Try {
      if (order.orderType == OrderType.BUY) matchAmount
      else {
        (BigDecimal(matchAmount) * matchPrice * BigDecimal(10).pow(priceDecimals - amountDecimals - 8)).toBigInt.bigInteger.longValueExact()
      }
    }.toEither.left.map(x => GenericError(x.getMessage))

  /** Calculates fee portfolio from the order (taking into account that in OrderV3 fee can be paid in asset != Waves)
    */
  private[diffs] def getOrderFeePortfolio(order: Order, fee: Long): Portfolio =
    Portfolio.build(order.matcherFeeAssetId, fee)
}
