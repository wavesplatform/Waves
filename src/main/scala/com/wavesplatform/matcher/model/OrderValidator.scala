package com.wavesplatform.matcher.model

import cats.implicits._
import com.wavesplatform.account.{Address, PublicKeyAccount}
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.DBUtils
import com.wavesplatform.matcher.api.DBUtils.indexes.active.MaxElements
import com.wavesplatform.matcher.model.OrderHistory.OrderInfoChange
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.state._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.transaction.{AssetAcc, AssetId}
import com.wavesplatform.utils.Time
import kamon.Kamon
import org.iq80.leveldb.DB

class OrderValidator(db: DB,
                     blockchain: Blockchain,
                     portfolio: Address => Portfolio,
                     validatePair: AssetPair => Either[String, AssetPair],
                     settings: MatcherSettings,
                     val matcherPublicKey: PublicKeyAccount,
                     time: Time) {
  import OrderValidator._

  private val MinExpiration: Long = 60 * 1000L

  private val timer = Kamon.timer("matcher.validation")

  private def spendableBalance(a: AssetAcc): Long = {
    val p = portfolio(a.account)
    a.assetId match {
      case Some(x) => p.assets.getOrElse(x, 0)
      case None    => p.spendableBalance
    }
  }

  private def validateBalance(o: Order): Either[String, Order] = {
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

  def tradableBalance(acc: AssetAcc): Long =
    timer
      .refine("action" -> "tradableBalance")
      .measure {
        math.max(0l, spendableBalance(acc) - DBUtils.openVolume(db, acc.account, acc.assetId))
      }

  def validateNewOrder(order: Order): Either[String, Order] =
    timer
      .refine("action" -> "place", "pair" -> order.assetPair.toString)
      .measure {
        lazy val senderAddress = order.sender.toAddress
        lazy val lowestOrderTs = DBUtils
          .lastOrderTimestamp(db, order.senderPublicKey)
          .getOrElse(settings.defaultOrderTimestamp) - settings.orderTimestampDrift

        for {
          _ <- (Right(order): Either[String, Order])
            .ensure("Incorrect matcher public key")(_.matcherPublicKey == matcherPublicKey)
            .ensure("Invalid address")(_ => !settings.blacklistedAddresses.contains(senderAddress))
            .ensure("Order expiration should be > 1 min")(_.expiration > time.correctedTime() + MinExpiration)
            .ensure(s"Order should have a timestamp after $lowestOrderTs, but it is ${order.timestamp}")(_.timestamp > 0)
            .ensure(s"Order matcherFee should be >= ${settings.minOrderFee}")(_.matcherFee >= settings.minOrderFee)
            .ensure("Invalid signature")(_.signatureValid())
            .ensure("Invalid order")(_.isValid(time.correctedTime()))
            .ensure("Order has already been placed")(o => DBUtils.orderInfo(db, o.id()).status == LimitOrder.NotFound)
            .ensure(s"Limit of $MaxElements active orders has been reached")(o => DBUtils.activeOrderCount(db, o.senderPublicKey) < MaxElements)
            .ensure("Trading on scripted account isn't allowed yet")(_ => !blockchain.hasScript(senderAddress))
          _ <- validateBalance(order)
          _ <- validatePair(order.assetPair)
        } yield order
      }
}

object OrderValidator {
  private def formatPortfolio(m: Map[Option[AssetId], Long]): String =
    m.map {
      case (assetId, v) => s"${AssetPair.assetIdStr(assetId)} -> $v"
    } mkString ("[", ", ", "]")
}
