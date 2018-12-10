package com.wavesplatform.matcher

import akka.actor.{Actor, ActorRef, Props, SupervisorStrategy, Terminated}
import com.wavesplatform.account.Address
import com.wavesplatform.matcher.model.Events
import com.wavesplatform.state.{EitherExt2, Portfolio}
import com.wavesplatform.utils.ScorexLogging

import scala.concurrent.duration._
import scala.collection.mutable
import scala.concurrent.Future

class AddressDirectory(portfolio: Address => Portfolio, matcherRef: ActorRef, settings: MatcherSettings) extends Actor with ScorexLogging {
  import AddressDirectory._
  import context._

  private[this] val children = mutable.AnyRefMap.empty[Address, ActorRef]

  override def supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private def createAddressActor(address: Address): ActorRef = {
    log.debug(s"Creating address actor for $address")
    watch(
      actorOf(
        Props(
          new AddressActor(address,
                           portfolio(address),
                           matcherRef,
                           settings.maxTimestampDiff,
                           5.seconds,
                           _ => Future.failed(new Exception),
                           _ => false)),
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
