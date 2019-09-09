package com.wavesplatform.state.diffs

import cats._
import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.{GenericError, OrderValidationError}
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order, OrderV3}

import scala.util.Right

object ExchangeTransactionDiff {

  def apply(blockchain: Blockchain, height: Int)(tx: ExchangeTransaction): Either[ValidationError, Diff] = {

    val matcher = tx.buyOrder.matcherPublicKey.toAddress
    val buyer   = tx.buyOrder.senderPublicKey.toAddress
    val seller  = tx.sellOrder.senderPublicKey.toAddress
    val assetIds =
      Set(tx.buyOrder.assetPair.amountAsset, tx.buyOrder.assetPair.priceAsset, tx.sellOrder.assetPair.amountAsset, tx.sellOrder.assetPair.priceAsset)
        .collect { case asset @ IssuedAsset(_) => asset }
    val assets             = assetIds.map(blockchain.assetDescription)
    val smartTradesEnabled = blockchain.activatedFeatures.contains(BlockchainFeatures.SmartAccountTrading.id)
    val smartAssetsEnabled = blockchain.activatedFeatures.contains(BlockchainFeatures.SmartAssets.id)

    for {
      _ <- Either.cond(assets.forall(_.isDefined), (), GenericError("Assets should be issued before they can be traded"))
      assetScripted = assets.count(_.flatMap(_.script).isDefined)
      _ <- Either.cond(
        smartAssetsEnabled || assetScripted == 0,
        (),
        GenericError(s"Smart assets can't participate in ExchangeTransactions (SmartAssetsFeature is disabled)")
      )
      buyerScripted = blockchain.hasScript(buyer)
      _ <- Either.cond(
        smartTradesEnabled || !buyerScripted,
        (),
        GenericError(s"Buyer $buyer can't participate in ExchangeTransaction because it has assigned Script (SmartAccountsTrades is disabled)")
      )
      sellerScripted = blockchain.hasScript(seller)
      _ <- Either.cond(
        smartTradesEnabled || !sellerScripted,
        (),
        GenericError(s"Seller $seller can't participate in ExchangeTransaction because it has assigned Script (SmartAccountsTrades is disabled)")
      )
      t                     <- enoughVolume(tx, blockchain)
      buyPriceAssetChange   <- t.buyOrder.getSpendAmount(t.amount, t.price).liftValidationError(tx).map(-_)
      buyAmountAssetChange  <- t.buyOrder.getReceiveAmount(t.amount, t.price).liftValidationError(tx)
      sellPriceAssetChange  <- t.sellOrder.getReceiveAmount(t.amount, t.price).liftValidationError(tx)
      sellAmountAssetChange <- t.sellOrder.getSpendAmount(t.amount, t.price).liftValidationError(tx).map(-_)
      scripts = {
        import com.wavesplatform.features.FeatureProvider._

        val addressScripted = Some(tx.sender.toAddress).count(blockchain.hasScript)

        // Don't count before Ride4DApps activation
        val ordersScripted = Seq(buyerScripted, sellerScripted)
          .filter(_ => blockchain.isFeatureActivated(BlockchainFeatures.Ride4DApps, height))
          .count(identity)

        assetScripted +
          addressScripted +
          ordersScripted
      }

      assetsComplexity =
        assetIds.toList
          .flatMap(blockchain.assetScriptWithComplexity)
          .map(_._2)

      accountsComplexity =
        List(tx.sender.toAddress, buyer, seller)
          .flatMap(blockchain.accountScriptWithComplexity)
          .map(_._2)

      scriptsComplexity = assetsComplexity.sum + accountsComplexity.sum
    } yield {

      def getAssetDiff(asset: Asset, buyAssetChange: Long, sellAssetChange: Long): Map[Address, Portfolio] = {
        Monoid.combine(
          Map(buyer  → getAssetPortfolio(asset, buyAssetChange)),
          Map(seller → getAssetPortfolio(asset, sellAssetChange)),
        )
      }

      val matcherPortfolio =
        Monoid.combineAll(
          Seq(
            getOrderFeePortfolio(t.buyOrder, t.buyMatcherFee),
            getOrderFeePortfolio(t.sellOrder, t.sellMatcherFee),
            wavesPortfolio(-t.fee),
          )
        )

      val feeDiff = Monoid.combineAll(
        Seq(
          Map(matcher -> matcherPortfolio),
          Map(buyer   -> getOrderFeePortfolio(t.buyOrder, -t.buyMatcherFee)),
          Map(seller  -> getOrderFeePortfolio(t.sellOrder, -t.sellMatcherFee))
        )
      )

      val priceDiff  = getAssetDiff(t.buyOrder.assetPair.priceAsset, buyPriceAssetChange, sellPriceAssetChange)
      val amountDiff = getAssetDiff(t.buyOrder.assetPair.amountAsset, buyAmountAssetChange, sellAmountAssetChange)
      val portfolios = Monoid.combineAll(Seq(feeDiff, priceDiff, amountDiff))

      Diff(
        height,
        tx,
        portfolios = portfolios,
        orderFills = Map(
          tx.buyOrder.id()  -> VolumeAndFee(tx.amount, tx.buyMatcherFee),
          tx.sellOrder.id() -> VolumeAndFee(tx.amount, tx.sellMatcherFee)
        ),
        scriptsRun = scripts,
        scriptsComplexity = scriptsComplexity
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
        case _: OrderV3 => BigInt(maxfee)
        case _          => BigInt(maxfee) * BigInt(amountTotal) / BigInt(maxAmount)
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

  def wavesPortfolio(amt: Long) = Portfolio(amt, LeaseBalance.empty, Map.empty)

  def getAssetPortfolio(asset: Asset, amt: Long): Portfolio = {
    asset.fold(wavesPortfolio(amt))(assetId => Portfolio(0, LeaseBalance.empty, Map(assetId -> amt)))
  }

  /*** Calculates fee portfolio from the order (taking into account that in OrderV3 fee can be paid in asset != Waves) */
  def getOrderFeePortfolio(order: Order, fee: Long): Portfolio = getAssetPortfolio(order.matcherFeeAssetId, fee)
}
