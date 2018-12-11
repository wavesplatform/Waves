package com.wavesplatform.matcher.model

import cats.implicits._
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FALSE, TRUE}
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.DBUtils
import com.wavesplatform.matcher.api.DBUtils.indexes.active.MaxElements
import com.wavesplatform.matcher.model.Events.OrderExecuted
import com.wavesplatform.matcher.model.OrderHistory.OrderInfoChange
import com.wavesplatform.matcher.smart.MatcherScriptRunner
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.state._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.Verifier
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.utils.Time
import kamon.Kamon
import org.iq80.leveldb.DB
import shapeless.Coproduct

import scala.util.control.NonFatal

/**
  * @param db matcher LevelDB instance
  * @param portfolio "pessimistic" portfolio, which includes pending spendings from UTX pool!
  */
class OrderValidator(db: DB,
                     blockchain: Blockchain,
                     transactionCreator: ExchangeTransactionCreator,
                     portfolio: Address => Portfolio,
                     validatePair: AssetPair => Either[String, AssetPair],
                     settings: MatcherSettings,
                     val matcherPublicKey: PublicKeyAccount,
                     time: Time) {
  import OrderValidator._

  private val timer                = Kamon.timer("matcher.validation")
  private val blacklistedAddresses = settings.blacklistedAddresses.map(Address.fromString(_).explicitGet())

  private def spendableBalance(a: AssetAcc): Long = {
    val p = portfolio(a.account)
    a.assetId match {
      case Some(x) => p.assets.getOrElse(x, 0)
      case None    => p.spendableBalance
    }
  }

  private def verifyOrderByAccountScript(address: Address, order: Order) =
    blockchain.accountScript(address).fold(verifySignature(order)) { script =>
      if (!blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height))
        Left("Trading on scripted account isn't allowed yet")
      else if (order.version <= 1) Left("Can't process order with signature from scripted account")
      else
        try MatcherScriptRunner[EVALUATED](script, order, isTokenScript = false) match {
          case (_, Left(execError)) => Left(s"Error executing script for $address: $execError")
          case (_, Right(FALSE))    => Left(s"Order rejected by script for $address")
          case (_, Right(TRUE))     => Right(order)
          case (_, Right(x))        => Left(s"Script returned not a boolean result, but $x")
        } catch {
          case NonFatal(e) => Left(s"Caught ${e.getClass.getCanonicalName} while executing script for $address: ${e.getMessage}")
        }
    }

  private def verifySmartToken(assetId: AssetId, tx: ExchangeTransaction) =
    blockchain.assetScript(assetId).fold[Either[String, Unit]](Right(())) { script =>
      if (!blockchain.isFeatureActivated(BlockchainFeatures.SmartAssets, blockchain.height))
        Left("Trading of scripted asset isn't allowed yet")
      else {
        try ScriptRunner[EVALUATED](blockchain.height, Coproduct(tx), blockchain, script, isTokenScript = true) match {
          case (_, Left(execError)) => Left(s"Error executing script of asset $assetId: $execError")
          case (_, Right(FALSE))    => Left(s"Order rejected by script of asset $assetId")
          case (_, Right(TRUE))     => Right(())
          case (_, Right(x))        => Left(s"Script returned not a boolean result, but $x")
        } catch {
          case NonFatal(e) => Left(s"Caught ${e.getClass.getCanonicalName} while executing script of asset $assetId: ${e.getMessage}")
        }
      }.left.map(_.toString)
    }

  private def validateBalance(o: Order): ValidationResult = {
    val senderAddress = o.sender.toAddress
    val lo            = LimitOrder(o)
    val actualBalance = Set(lo.feeAsset, lo.spentAsset).map(assetId => assetId -> spendableBalance(AssetAcc(senderAddress, assetId))).toMap
    val openVolume    = actualBalance.map { case (assetId, _) => assetId -> DBUtils.openVolume(db, senderAddress, assetId) }
    val change        = OrderInfoChange(o, None, OrderInfo(o.amount, 0L, None, None, o.matcherFee, Some(0L)))
    val newOrder      = OrderHistory.diff(List(change)).getOrElse(senderAddress, OpenPortfolio.empty)
    val needs         = OpenPortfolio(openVolume).combine(newOrder)

    Either.cond(
      actualBalance.combine(needs.orders.mapValues(-_)).forall(_._2 >= 0),
      o,
      s"Not enough tradable balance. Order requires ${formatPortfolio(newOrder.orders)}, " +
        s"available balance is ${formatPortfolio(actualBalance.combine(openVolume.mapValues(-_)))}"
    )
  }

  @inline private def decimals(assetId: Option[AssetId]) = assetId.fold[Either[String, Int]](Right(8)) { aid =>
    blockchain.assetDescription(aid).map(_.decimals).toRight(s"Invalid asset id $aid")
  }

  private def validateDecimals(o: Order): ValidationResult =
    for {
      pd <- decimals(o.assetPair.priceAsset)
      ad <- decimals(o.assetPair.amountAsset)
      insignificantDecimals = (pd - ad).max(0)
      _ <- Either.cond(o.price % BigDecimal(10).pow(insignificantDecimals).toLongExact == 0,
                       o,
                       s"Invalid price, last $insignificantDecimals digits must be 0")
    } yield o

  def tradableBalance(acc: AssetAcc): Long =
    timer
      .refine("action" -> "tradableBalance")
      .measure {
        math.max(0l, spendableBalance(acc) - DBUtils.openVolume(db, acc.account, acc.assetId))
      }

  def validateNewOrder(order: Order): ValidationResult =
    timer
      .refine("action" -> "place", "pair" -> order.assetPair.toString)
      .measure {
        lazy val senderAddress = order.sender.toAddress
        lazy val lowestOrderTs = DBUtils
          .lastOrderTimestamp(db, order.senderPublicKey)
          .getOrElse(settings.defaultOrderTimestamp) - settings.orderTimestampDrift

        lazy val minOrderFee: Long =
          ExchangeTransactionCreator.getMinFee(blockchain, settings.orderMatchTxFee, matcherPublicKey, Some(order.sender), None, order.assetPair)

        lazy val exchangeTx = {
          val fakeOrder: Order = order match {
            case x: OrderV1 => x.copy(orderType = x.orderType.opposite)
            case x: OrderV2 => x.copy(orderType = x.orderType.opposite)
          }
          transactionCreator.createTransaction(OrderExecuted(LimitOrder(fakeOrder), LimitOrder(order))).left.map(_.toString)
        }

        def verifyAssetScript(assetId: Option[AssetId]) = assetId.fold[ValidationResult](Right(order)) { assetId =>
          exchangeTx.flatMap(verifySmartToken(assetId, _)).right.map(_ => order)
        }

        for {
          _ <- (Right(order): ValidationResult)
            .ensure("Orders of version 1 are only accepted, because SmartAccountTrading has not been activated yet")(
              _.version == 1 || blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height))
            .ensure("Incorrect matcher public key")(_.matcherPublicKey == matcherPublicKey)
            .ensure("Invalid address")(_ => !blacklistedAddresses.contains(senderAddress))
            .ensure("Order expiration should be > 1 min")(_.expiration > time.correctedTime() + MinExpiration)
            .ensure(s"Order should have a timestamp after $lowestOrderTs, but it is ${order.timestamp}")(_.timestamp > lowestOrderTs)
            .ensure(s"Order matcherFee should be >= $minOrderFee")(_.matcherFee >= minOrderFee)
          _ <- order.isValid(time.correctedTime()).toEither
          _ <- (Right(order): ValidationResult)
            .ensure("Order has already been placed")(o => DBUtils.orderInfo(db, o.id()).status == LimitOrder.NotFound)
            .ensure(s"Limit of $MaxElements active orders has been reached")(o => DBUtils.activeOrderCount(db, o.senderPublicKey) < MaxElements)
          _ <- validateBalance(order)
          _ <- validatePair(order.assetPair)
          _ <- validateDecimals(order)
          _ <- verifyOrderByAccountScript(order.sender, order)
          _ <- verifyAssetScript(order.assetPair.amountAsset)
          _ <- verifyAssetScript(order.assetPair.priceAsset)
        } yield order
      }
}

object OrderValidator {

  type ValidationResult = Either[String, Order]

  val MinExpiration: Long = 60 * 1000L

  private def formatPortfolio(m: Map[Option[AssetId], Long]): String =
    m.map {
      case (assetId, v) => s"${AssetPair.assetIdStr(assetId)} -> $v"
    } mkString ("[", ", ", "]")

  private def verifySignature(order: Order): ValidationResult =
    Verifier
      .verifyAsEllipticCurveSignature(order)
      .leftMap(_.toString)
}
