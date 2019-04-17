package com.wavesplatform.matcher.market

import akka.actor.{Actor, Props}
import com.wavesplatform.matcher.MatcherSettings.ExchangeTransactionBroadcastSettings
import com.wavesplatform.matcher.market.ExchangeTransactionBroadcastActor._
import com.wavesplatform.matcher.model.Events.ExchangeTransactionCreated
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.utils.{ScorexLogging, Time}

class ExchangeTransactionBroadcastActor(settings: ExchangeTransactionBroadcastSettings,
                                        time: Time,
                                        isValid: ExchangeTransaction => Boolean,
                                        isConfirmed: ExchangeTransaction => Boolean,
                                        broadcast: Seq[ExchangeTransaction] => Unit)
    extends Actor
    with ScorexLogging {

  import context.dispatcher

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ExchangeTransactionCreated])
    scheduleSend()
  }

  private val default: Receive = {
    case ExchangeTransactionCreated(tx) => if (isValid(tx)) broadcast(List(tx))
  }

  private def watching(toCheck: Vector[ExchangeTransaction], next: Vector[ExchangeTransaction]): Receive = {
    case ExchangeTransactionCreated(tx) =>
      if (isValid(tx)) {
        broadcast(List(tx))
        context.become(watching(toCheck, next :+ tx))
      }

    case Send =>
      val nowMs    = time.getTimestamp()
      val expireMs = nowMs - settings.maxPendingTime.toMillis

      val (confirmed, unconfirmed) = toCheck.partition(isConfirmed)
      val (expired, ready)         = unconfirmed.partition(_.timestamp <= expireMs)

      broadcast(ready)
      log.debug(
        s"Stats: ${confirmed.size} confirmed, ${ready.size} sent, ${expired.size} failed to send: ${expired.map(_.id().base58).mkString(", ")}")

      scheduleSend()
      context.become(watching(next ++ ready, Vector.empty))
  }

  override val receive: Receive = if (settings.broadcastUntilConfirmed) watching(toCheck = Vector.empty, next = Vector.empty) else default

  private def scheduleSend(): Unit = context.system.scheduler.scheduleOnce(settings.interval, self, Send)
}

object ExchangeTransactionBroadcastActor {
  object Send

  def props(settings: ExchangeTransactionBroadcastSettings,
            time: Time,
            isValid: ExchangeTransaction => Boolean,
            isConfirmed: ExchangeTransaction => Boolean,
            broadcast: Seq[ExchangeTransaction] => Unit): Props =
    Props(new ExchangeTransactionBroadcastActor(settings, time, isValid, isConfirmed, broadcast))
}
