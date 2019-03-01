package com.wavesplatform.matcher.model

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.v1.compiler.Terms.{FALSE, TRUE}
import com.wavesplatform.matcher.smart.MatcherScriptRunner
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.state._
import com.wavesplatform.transaction._
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

  private def verifySmartToken(blockchain: Blockchain, assetId: AssetId, tx: ExchangeTransaction) =
    blockchain.assetScript(assetId).fold[Either[String, Unit]](Right(())) { script =>
      if (!blockchain.isFeatureActivated(BlockchainFeatures.SmartAssets, blockchain.height))
        Left("Trading of scripted asset isn't allowed yet")
      else {
        try ScriptRunner(blockchain.height, Coproduct(tx), blockchain, script, isTokenScript = true) match {
          case (_, Left(execError)) => Left(s"Error executing script of asset $assetId: $execError")
          case (_, Right(FALSE))    => Left(s"Order rejected by script of asset $assetId")
          case (_, Right(TRUE))     => Right(())
          case (_, Right(x))        => Left(s"Script returned not a boolean result, but $x")
        } catch {
          case NonFatal(e) => Left(s"Caught ${e.getClass.getCanonicalName} while executing script of asset $assetId: ${e.getMessage}")
        }
      }.left.map(_.toString)
    }

  @inline private def decimals(blockchain: Blockchain, assetId: Option[AssetId]) = assetId.fold[Either[String, Int]](Right(8)) { aid =>
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
      orderMatchTxFee: Long,
      matcherAddress: Address,
      time: Time,
  )(order: Order): ValidationResult = timer.measure {
    lazy val exchangeTx = {
      val fakeOrder: Order = order match {
        case x: OrderV1 => x.copy(orderType = x.orderType.opposite)
        case x: OrderV2 => x.copy(orderType = x.orderType.opposite)
      }
      transactionCreator(LimitOrder(fakeOrder), LimitOrder(order), time.correctedTime()).left.map(_.toString)
    }

    def verifyAssetScript(assetId: Option[AssetId]) = assetId.fold[ValidationResult](Right(order)) { assetId =>
      exchangeTx.flatMap(verifySmartToken(blockchain, assetId, _)).right.map(_ => order)
    }

    for {
      _ <- (Right(order): ValidationResult)
        .ensure("Orders of version 1 are only accepted, because SmartAccountTrading has not been activated yet")(
          _.version == 1 || blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height))
        .ensure("Order expiration should be > 1 min")(_.expiration > time.correctedTime() + MinExpiration)
      mof = ExchangeTransactionCreator.minFee(blockchain, orderMatchTxFee, matcherAddress, order.assetPair)
      _ <- (Right(order): ValidationResult).ensure(s"Order matcherFee should be >= $mof")(_.matcherFee >= mof)
      _ <- validateDecimals(blockchain, order)
      _ <- verifyOrderByAccountScript(blockchain, order.sender, order)
      _ <- verifyAssetScript(order.assetPair.amountAsset)
      _ <- verifyAssetScript(order.assetPair.priceAsset)
    } yield order
  }

  private def formatBalance(b: Map[Option[AssetId], Long]): String =
    b.map { case (k, v) => s"${AssetPair.assetIdStr(k)}:$v" } mkString ("{", ", ", "}")

  private def validateBalance(order: Order, tradableBalance: Option[AssetId] => Long): ValidationResult = {
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

  def matcherSettingsAware(
      matcherPublicKey: PublicKeyAccount,
      blacklistedAddresses: Set[Address],
      blacklistedAssets: Set[Option[AssetId]],
  )(order: Order): ValidationResult = {
    for {
      _ <- (Right(order): ValidationResult)
        .ensure("Incorrect matcher public key")(_.matcherPublicKey == matcherPublicKey)
        .ensure("Invalid address")(_ => !blacklistedAddresses.contains(order.sender.toAddress))
        .ensure(s"Invalid amount asset ${order.assetPair.amountAsset}")(_ => !blacklistedAssets(order.assetPair.amountAsset))
        .ensure(s"Invalid price asset ${order.assetPair.priceAsset}")(_ => !blacklistedAssets(order.assetPair.priceAsset))
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
      tradableBalance: Option[AssetId] => Long,
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
