package com.wavesplatform.matcher.market

import akka.actor.{Actor, Props}
import com.wavesplatform.matcher.MatcherSettings.BroadcastUntilConfirmedSettings
import com.wavesplatform.matcher.market.BroadcastUntilConfirmedActor._
import com.wavesplatform.matcher.model.Events.ExchangeTransactionCreated
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.utils.{ScorexLogging, Time}

class BroadcastUntilConfirmedActor(settings: BroadcastUntilConfirmedSettings,
                                   time: Time,
                                   isConfirmed: ExchangeTransaction => Boolean,
                                   broadcast: Seq[ExchangeTransaction] => Unit)
    extends Actor
    with ScorexLogging {

  import context.dispatcher

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ExchangeTransactionCreated])
    scheduleSend()
  }

  override def receive: Receive = state(toCheck = Vector.empty, next = Vector.empty)

  private def state(toCheck: Vector[ExchangeTransaction], next: Vector[ExchangeTransaction]): Receive = {
    case ExchangeTransactionCreated(tx) =>
      broadcast(List(tx))
      context.become(state(toCheck, next :+ tx))

    case Send =>
      val nowMs    = time.getTimestamp()
      val expireMs = nowMs - settings.maxPendingTime.toMillis

      val (confirmed, unconfirmed) = toCheck.partition(isConfirmed)
      val (expired, ready)         = unconfirmed.partition(_.timestamp <= expireMs)

      broadcast(ready)
      log.debug(
        s"Stats: ${confirmed.size} confirmed, ${ready.size} sent, ${expired.size} failed to send: ${expired.map(_.id().base58).mkString(", ")}")

      scheduleSend()
      context.become(state(next, ready))
  }

  private def scheduleSend(): Unit = context.system.scheduler.scheduleOnce(settings.interval, self, Send)
}

object BroadcastUntilConfirmedActor {
  object Send

  def props(settings: BroadcastUntilConfirmedSettings,
            time: Time,
            isConfirmed: ExchangeTransaction => Boolean,
            broadcast: Seq[ExchangeTransaction] => Unit): Props = Props(new BroadcastUntilConfirmedActor(settings, time, isConfirmed, broadcast))
}
