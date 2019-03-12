package com.wavesplatform.matcher.model

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.v1.compiler.Terms.{FALSE, TRUE}
import com.wavesplatform.matcher.smart.MatcherScriptRunner
import com.wavesplatform.matcher.util._
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.settings.fee.AssetType
import com.wavesplatform.settings.fee.AssetType.AssetType
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

import scala.util.control.NonFatal

object OrderValidator {

  type ValidationResult = Either[String, Order]

  private val timer = Kamon.timer("matcher.validation").refine("type" -> "blockchain")

  val MinExpiration: Long   = 60 * 1000L
  val MaxActiveOrders: Long = 200

  private def verifySignature(order: Order): ValidationResult =
    Verifier
      .verifyAsEllipticCurveSignature(order)
      .leftMap(_.toString)

  private def verifyOrderByAccountScript(blockchain: Blockchain, address: Address, order: Order) =
    blockchain.accountScript(address).fold(verifySignature(order)) { script =>
      if (!blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height))
        Left("Trading on scripted account isn't allowed yet")
      else if (order.version <= 1) Left("Can't process order with signature from scripted account")
      else
        try MatcherScriptRunner(script, order, isTokenScript = false) match {
          case (_, Left(execError)) => Left(s"Error executing script for $address: $execError")
          case (_, Right(FALSE))    => Left(s"Order rejected by script for $address")
          case (_, Right(TRUE))     => Right(order)
          case (_, Right(x))        => Left(s"Script returned not a boolean result, but $x")
        } catch {
          case NonFatal(e) => Left(s"Caught ${e.getClass.getCanonicalName} while executing script for $address: ${e.getMessage}")
        }
    }

  private def verifySmartToken(blockchain: Blockchain, asset: IssuedAsset, tx: ExchangeTransaction) =
    blockchain.assetScript(asset).fold[Either[String, Unit]](Right(())) { script =>
      if (!blockchain.isFeatureActivated(BlockchainFeatures.SmartAssets, blockchain.height))
        Left("Trading of scripted asset isn't allowed yet")
      else {
        try ScriptRunner(blockchain.height, Coproduct(tx), blockchain, script, isTokenScript = true) match {
          case (_, Left(execError)) => Left(s"Error executing script of asset $asset: $execError")
          case (_, Right(FALSE))    => Left(s"Order rejected by script of asset $asset")
          case (_, Right(TRUE))     => Right(())
          case (_, Right(x))        => Left(s"Script returned not a boolean result, but $x")
        } catch {
          case NonFatal(e) => Left(s"Caught ${e.getClass.getCanonicalName} while executing script of asset $asset: ${e.getMessage}")
        }
      }.left.map(_.toString)
    }

  @inline private def decimals(blockchain: Blockchain, assetId: Asset) = assetId.fold[Either[String, Int]](Right(8)) { aid =>
    blockchain.assetDescription(aid).map(_.decimals).toRight(s"Invalid asset id $aid")
  }

  private def validateDecimals(blockchain: Blockchain, o: Order): ValidationResult =
    for {
      pd <- decimals(blockchain, o.assetPair.priceAsset)
      ad <- decimals(blockchain, o.assetPair.amountAsset)
      insignificantDecimals = (pd - ad).max(0)
      _ <- Either.cond(o.price % BigDecimal(10).pow(insignificantDecimals).toLongExact == 0,
                       o,
                       s"Invalid price, last $insignificantDecimals digits must be 0")
    } yield o

  def blockchainAware(
      blockchain: Blockchain,
      transactionCreator: (LimitOrder, LimitOrder, Long) => Either[ValidationError, ExchangeTransaction],
      matcherAddress: Address,
      time: Time,
      orderFeeSettings: OrderFeeSettings
  )(order: Order): ValidationResult = timer.measure {

    lazy val exchangeTx = {
      val fakeOrder = order.updateType(order.orderType.opposite)
      transactionCreator(LimitOrder(fakeOrder), LimitOrder(order), time.correctedTime()).leftMap(_.toString)
    }

    def verifyAssetScript(assetId: Asset) = {
      assetId match {
        case Waves => Right(order)
        case asset @ IssuedAsset(_) =>
          exchangeTx.flatMap(verifySmartToken(blockchain, asset, _)).right.map(_ => order)
      }
    }

    for {
      _ <- (Right(order): ValidationResult)
        .ensure("Orders of version 1 are only accepted, because SmartAccountTrading has not been activated yet")(
          _.version == 1 || blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height))
        .ensure("Orders of version 3 have not been activated yet")(
          _.version != 3 || blockchain.isFeatureActivated(BlockchainFeatures.OrderV3, blockchain.height))
        .ensure("Order expiration should be > 1 min")(_.expiration > time.correctedTime() + MinExpiration)
      mof = ExchangeTransactionCreator.minFee(blockchain, matcherAddress, order.assetPair)
      _ <- (Right(order): ValidationResult).ensure(s"Order matcherFee should be >= $mof") { order =>
        orderFeeSettings match {
          case _: FixedWavesSettings => order.matcherFee >= mof
          case _                     => true
        }
      }
      _ <- validateDecimals(blockchain, order)
      _ <- verifyOrderByAccountScript(blockchain, order.sender, order)
      _ <- verifyAssetScript(order.assetPair.amountAsset)
      _ <- verifyAssetScript(order.assetPair.priceAsset)
    } yield order
  }

  private def formatBalance(b: Map[Asset, Long]): String =
    b.map { case (k, v) => s"${AssetPair.assetIdStr(k)}:$v" } mkString ("{", ", ", "}")

  private def validateBalance(order: Order, tradableBalance: Asset => Long): ValidationResult = {
    val lo               = LimitOrder(order)
    val requiredForOrder = lo.requiredBalance

    val available = requiredForOrder.keySet.map { assetId =>
      assetId -> tradableBalance(assetId)
    }.toMap

    val negativeBalances = Monoid.combine(available, requiredForOrder.mapValues(-_)).filter(_._2 < 0)

    Either.cond(
      negativeBalances.isEmpty,
      order,
      s"Not enough tradable balance. Order requires ${formatBalance(requiredForOrder)}, but only ${formatBalance(available)} is available"
    )
  }

  private[matcher] def getValidFeeAsset(order: Order, assetType: AssetType): Option[AssetId] = {
    assetType match {
      case AssetType.AMOUNT    => order.assetPair.amountAsset
      case AssetType.PRICE     => order.assetPair.priceAsset
      case AssetType.RECEIVING => order.getReceiveAssetId
      case AssetType.SPENDING  => order.getSpendAssetId
    }
  }

  private[matcher] def getMinValidFee(order: Order, percentSettings: PercentSettings): Long = {

    lazy val receiveAmount = order.getReceiveAmount(order.amount, order.price).explicitGet()
    lazy val spentAmount   = order.getSpendAmount(order.amount, order.price).explicitGet()

    val amountFactor = percentSettings.assetType match {
      case AssetType.AMOUNT    => order.amount
      case AssetType.PRICE     => if (order.orderType == OrderType.BUY) spentAmount else receiveAmount
      case AssetType.RECEIVING => receiveAmount
      case AssetType.SPENDING  => spentAmount
    }

    multiplyLongByDouble(amountFactor, percentSettings.minFee / 100)
  }

  def validateOrderFee(order: Order, orderFeeSettings: OrderFeeSettings): ValidationResult = {
    for {
      _ <- (Right(order): ValidationResult)
        .ensure(s"Matcher's fee asset in order (${order.matcherFeeAssetId}) does not meet matcher's settings requirements") { order =>
          val isMatcherFeeAssetValid = orderFeeSettings match {
            case _: FixedWavesSettings            => order.matcherFeeAssetId.isEmpty
            case FixedSettings(defaultAssetId, _) => order.matcherFeeAssetId == defaultAssetId
            case PercentSettings(assetType, _)    => order.matcherFeeAssetId == getValidFeeAsset(order, assetType)
          }
          order.version != 3 || isMatcherFeeAssetValid
        }
      _ <- (Right(order): ValidationResult)
        .ensure(s"Matcher's fee (${order.matcherFee}) is less than minimally admissible one") { order =>
          val isMatcherFeeValid = orderFeeSettings match {
            case FixedWavesSettings(wavesMinFee)  => order.matcherFee >= wavesMinFee
            case FixedSettings(_, fixedMinFee)    => order.matcherFee >= fixedMinFee
            case percentSettings: PercentSettings => order.matcherFee >= getMinValidFee(order, percentSettings)
          }
          order.version != 3 || isMatcherFeeValid
        }

    } yield order
  }

  def matcherSettingsAware(
      matcherPublicKey: PublicKeyAccount,
      blacklistedAddresses: Set[Address],
      blacklistedAssets: Set[Asset],
      orderFeeSettings: OrderFeeSettings
  )(order: Order): ValidationResult = {
    for {
      _ <- (Right(order): ValidationResult)
        .ensure("Incorrect matcher public key")(_.matcherPublicKey == matcherPublicKey)
        .ensure("Invalid address")(_ => !blacklistedAddresses.contains(order.sender.toAddress))
        .ensure(s"Invalid amount asset ${order.assetPair.amountAsset}")(_ => !blacklistedAssets(order.assetPair.amountAsset))
        .ensure(s"Invalid price asset ${order.assetPair.priceAsset}")(_ => !blacklistedAssets(order.assetPair.priceAsset))
        .ensure(s"Invalid fee asset ${order.matcherFeeAssetId} (blacklisted)")(_ => order.version != 3 || !blacklistedAssets(order.matcherFeeAssetId))
      _ <- validateOrderFee(order, orderFeeSettings)
    } yield order
  }

  def timeAware(time: Time)(order: Order): ValidationResult = {
    for {
      _ <- (Right(order): ValidationResult)
        .ensure("Order expiration should be > 1 min")(_.expiration > time.correctedTime() + MinExpiration)
      _ <- order.isValid(time.correctedTime()).toEither
    } yield order
  }

  def accountStateAware(
      sender: Address,
      tradableBalance: Asset => Long,
      activeOrderCount: => Int,
      orderExists: ByteStr => Boolean,
  )(order: Order): ValidationResult =
    for {
      _ <- (Right(order): ValidationResult)
        .ensure(s"Order sender ${order.sender.toAddress} does not match expected $sender")(_.sender.toAddress == sender)
        .ensure(s"Limit of $MaxActiveOrders active orders has been reached")(_ => activeOrderCount < MaxActiveOrders)
        .ensure("Order has already been placed")(o => !orderExists(o.id()))
      _ <- validateBalance(order, tradableBalance)
    } yield order
}
