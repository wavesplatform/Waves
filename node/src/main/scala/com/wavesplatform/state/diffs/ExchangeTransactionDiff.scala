package com.wavesplatform.state.diffs

import cats._
import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{GenericError, OrderValidationError}
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order, OrderType}
import com.wavesplatform.transaction.{Asset, TxVersion}

import scala.util.{Right, Try}

object ExchangeTransactionDiff {

  def apply(blockchain: Blockchain)(tx: ExchangeTransaction): Either[ValidationError, Diff] = {
    val matcher = tx.buyOrder.matcherPublicKey.toAddress
    val buyer   = tx.buyOrder.senderPublicKey.toAddress
    val seller  = tx.sellOrder.senderPublicKey.toAddress

    val assetIds =
      List(tx.buyOrder.assetPair.amountAsset, tx.buyOrder.assetPair.priceAsset, tx.sellOrder.assetPair.amountAsset, tx.sellOrder.assetPair.priceAsset).collect {
        case asset: IssuedAsset => asset
      }.distinct
    val assets = assetIds.map(id => id -> blockchain.assetDescription(id)).toMap

    val smartTradesEnabled = blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading)
    val smartAssetsEnabled = blockchain.isFeatureActivated(BlockchainFeatures.SmartAssets)

    def isPriceValid(amountDecimals: Int, priceDecimals: Int) = {
      def convertPrice(price: Long, amountDecimals: Int, priceDecimals: Int) =
        Try {
          (BigDecimal(price) / BigDecimal(10).pow(priceDecimals - amountDecimals)).toBigInt.bigInteger.longValueExact()
        }.toEither.leftMap(x => GenericError(x.getMessage))

      def orderPrice(order: Order, amountDecimals: Int, priceDecimals: Int) =
        if (tx.version >= TxVersion.V3 && order.version < Order.V4) convertPrice(order.price, amountDecimals, priceDecimals)
        else Right(order.price)

      for {
        _              <- Either.cond(tx.price != 0L, (), GenericError("price should be > 0"))
        buyOrderPrice  <- orderPrice(tx.buyOrder, amountDecimals, priceDecimals)
        sellOrderPrice <- orderPrice(tx.sellOrder, amountDecimals, priceDecimals)
        _              <- Either.cond(tx.price <= buyOrderPrice, (), GenericError("price should be <= buyOrder.price"))
        _              <- Either.cond(tx.price >= sellOrderPrice, (), GenericError("price should be >= sellOrder.price"))
      } yield ()
    }

    for {
      _ <- Either.cond(assets.values.forall(_.isDefined), (), GenericError("Assets should be issued before they can be traded"))
      assetScripted = assets.values.count(_.flatMap(_.script).isDefined)
      _ <- Either.cond(
        smartAssetsEnabled || assetScripted == 0,
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
      amountDecimals = if (tx.version < TxVersion.V3) 8 else tx.buyOrder.assetPair.amountAsset.fold(8)(ia => assets(ia).get.decimals)
      priceDecimals  = if (tx.version < TxVersion.V3) 8 else tx.buyOrder.assetPair.priceAsset.fold(8)(ia => assets(ia).get.decimals)
      _                     <- isPriceValid(amountDecimals, priceDecimals)
      tx                    <- enoughVolume(tx, blockchain)
      buyPriceAssetChange   <- getSpendAmount(tx.buyOrder, amountDecimals, priceDecimals, tx.amount, tx.price).map(-_)
      buyAmountAssetChange  <- getReceiveAmount(tx.buyOrder, amountDecimals, priceDecimals, tx.amount, tx.price)
      sellPriceAssetChange  <- getReceiveAmount(tx.sellOrder, amountDecimals, priceDecimals, tx.amount, tx.price)
      sellAmountAssetChange <- getSpendAmount(tx.sellOrder, amountDecimals, priceDecimals, tx.amount, tx.price).map(-_)
      scripts = {
        val addressScripted = Some(tx.sender.toAddress).count(blockchain.hasAccountScript)

        // Don't count before Ride4DApps activation
        val ordersScripted = Seq(buyerScripted, sellerScripted)
          .filter(_ => blockchain.isFeatureActivated(BlockchainFeatures.Ride4DApps, blockchain.height))
          .count(identity)

        assetScripted +
          addressScripted +
          ordersScripted
      }
    } yield {

      def getAssetDiff(asset: Asset, buyAssetChange: Long, sellAssetChange: Long): Map[Address, Portfolio] = {
        Monoid.combine(
          Map(buyer  -> Portfolio.build(asset, buyAssetChange)),
          Map(seller -> Portfolio.build(asset, sellAssetChange))
        )
      }

      val matcherPortfolio =
        Monoid.combineAll(
          Seq(
            getOrderFeePortfolio(tx.buyOrder, tx.buyMatcherFee),
            getOrderFeePortfolio(tx.sellOrder, tx.sellMatcherFee),
            Portfolio.waves(-tx.fee)
          )
        )

      val feeDiff = Monoid.combineAll(
        Seq(
          Map(matcher -> matcherPortfolio),
          Map(buyer   -> getOrderFeePortfolio(tx.buyOrder, -tx.buyMatcherFee)),
          Map(seller  -> getOrderFeePortfolio(tx.sellOrder, -tx.sellMatcherFee))
        )
      )

      val priceDiff  = getAssetDiff(tx.buyOrder.assetPair.priceAsset, buyPriceAssetChange, sellPriceAssetChange)
      val amountDiff = getAssetDiff(tx.buyOrder.assetPair.amountAsset, buyAmountAssetChange, sellAmountAssetChange)
      val portfolios = Monoid.combineAll(Seq(feeDiff, priceDiff, amountDiff))

      Diff(
        portfolios = portfolios,
        orderFills = Map(
          tx.buyOrder.id()  -> VolumeAndFee(tx.amount, tx.buyMatcherFee),
          tx.sellOrder.id() -> VolumeAndFee(tx.amount, tx.sellMatcherFee)
        ),
        scriptsRun = scripts
      )
    }
  }

  private def enoughVolume(exTrans: ExchangeTransaction, blockchain: Blockchain): Either[ValidationError, ExchangeTransaction] = {

    val filledBuy  = blockchain.filledVolumeAndFee(exTrans.buyOrder.id())
    val filledSell = blockchain.filledVolumeAndFee(exTrans.sellOrder.id())

    val buyTotal  = filledBuy.volume + exTrans.amount
    val sellTotal = filledSell.volume + exTrans.amount

    lazy val buyAmountValid  = exTrans.buyOrder.amount >= buyTotal
    lazy val sellAmountValid = exTrans.sellOrder.amount >= sellTotal

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
        maxfee = exTrans.buyOrder.matcherFee,
        maxAmount = exTrans.buyOrder.amount,
        order = exTrans.buyOrder
      )

    lazy val sellFeeValid =
      isFeeValid(
        feeTotal = filledSell.fee + exTrans.sellMatcherFee,
        amountTotal = sellTotal,
        maxfee = exTrans.sellOrder.matcherFee,
        maxAmount = exTrans.sellOrder.amount,
        order = exTrans.sellOrder
      )

    if (!buyAmountValid) Left(OrderValidationError(exTrans.buyOrder, s"Too much buy. Already filled volume for the order: ${filledBuy.volume}"))
    else if (!sellAmountValid)
      Left(OrderValidationError(exTrans.sellOrder, s"Too much sell. Already filled volume for the order: ${filledSell.volume}"))
    else if (!buyFeeValid) Left(OrderValidationError(exTrans.buyOrder, s"Insufficient buy fee"))
    else if (!sellFeeValid) Left(OrderValidationError(exTrans.sellOrder, s"Insufficient sell fee"))
    else Right(exTrans)
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
        if (order.getSpendAssetId == Waves && !(spend + order.matcherFee).isValidLong) {
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

  /**
    * Calculates fee portfolio from the order (taking into account that in OrderV3 fee can be paid in asset != Waves)
    */
  private[diffs] def getOrderFeePortfolio(order: Order, fee: Long): Portfolio =
    Portfolio.build(order.matcherFeeAssetId, fee)
}
