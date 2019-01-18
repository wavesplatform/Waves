package com.wavesplatform.matcher

import akka.actor.{Actor, ActorRef, Props, SupervisorStrategy, Terminated}
import com.wavesplatform.account.Address
import com.wavesplatform.matcher.Matcher.StoreEvent
import com.wavesplatform.matcher.model.Events
import com.wavesplatform.state.{EitherExt2, Portfolio}
import com.wavesplatform.utils.ScorexLogging
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.collection.mutable
import scala.concurrent.duration._

class AddressDirectory(portfolioChanged: Observable[Address],
                       portfolio: Address => Portfolio,
                       storeEvent: StoreEvent,
                       settings: MatcherSettings,
                       orderDB: OrderDB)
    extends Actor
    with ScorexLogging {
  import AddressDirectory._
  import context._

  private[this] val children = mutable.AnyRefMap.empty[Address, ActorRef]

  portfolioChanged
    .filter(children.contains)
    .bufferTimed(settings.balanceWatchingBufferInterval)
    .filter(_.nonEmpty)
    .foreach(_.toSet.foreach((address: Address) => children.get(address).foreach(_ ! AddressActor.BalanceUpdated)))(Scheduler(context.dispatcher))

  override def supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private def createAddressActor(address: Address): ActorRef = {
    log.debug(s"Creating address actor for $address")
    watch(
      actorOf(
        Props(new AddressActor(address, portfolio(address), settings.maxTimestampDiff, 5.seconds, orderDB, storeEvent)),
        address.toString
      ))
  }

  private def forward(address: Address, msg: Any): Unit = {
    val handler = children.getOrElseUpdate(address, createAddressActor(address))
    log.trace(s"Forwarding $msg to $handler")
    handler.forward(msg)
  }

  override def receive: Receive = {
    case Envelope(address, cmd) =>
      forward(address, cmd)

    case e @ Events.OrderAdded(lo) =>
      forward(lo.order.sender, e)
    case e @ Events.OrderExecuted(submitted, counter) =>
      forward(submitted.order.sender, e)
      forward(counter.order.sender, e)
    case e @ Events.OrderCanceled(lo, _) =>
      forward(lo.order.sender, e)

    case Terminated(child) =>
      val addressString = child.path.name
      val address       = Address.fromString(addressString).explicitGet()
      children.remove(address)
      log.warn(s"Address handler for $addressString terminated")
  }
}

object AddressDirectory {
  case class Envelope(address: Address, cmd: AddressActor.Command)
}
