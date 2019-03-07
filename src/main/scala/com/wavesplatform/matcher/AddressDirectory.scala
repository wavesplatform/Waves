package com.wavesplatform.matcher

import akka.actor.{Actor, ActorRef, Props, SupervisorStrategy, Terminated}
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.matcher.model.Events
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.collection.mutable

class AddressDirectory(spendableBalanceChanged: Observable[(Address, Asset)], settings: MatcherSettings, addressActorProps: Address => Props)
    extends Actor
    with ScorexLogging {
  import AddressDirectory._
  import context._

  private[this] val children = mutable.AnyRefMap.empty[Address, ActorRef]

  spendableBalanceChanged
    .filter(x => children.contains(x._1))
    .bufferTimed(settings.balanceWatchingBufferInterval)
    .filter(_.nonEmpty)
    .foreach { changes =>
      val acc = mutable.Map.empty[Address, Set[Asset]]
      changes.foreach { case (addr, changed) => acc.update(addr, acc.getOrElse(addr, Set.empty) + changed) }

      acc.foreach { case (addr, changedAssets) => children.get(addr).foreach(_ ! AddressActor.BalanceUpdated(changedAssets)) }
    }(Scheduler(context.dispatcher))

  override def supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private def createAddressActor(address: Address): ActorRef = {
    log.debug(s"Creating address actor for $address")
    watch(actorOf(addressActorProps(address), address.toString))
  }

  private def forward(address: Address, msg: Any): Unit = {
    val handler = children.getOrElseUpdate(address, createAddressActor(address))
    handler.forward(msg)
  }

  override def receive: Receive = {
    case Envelope(address, cmd) =>
      forward(address, cmd)

    case e @ Events.OrderAdded(lo) =>
      forward(lo.order.sender, e)
    case e @ Events.OrderExecuted(submitted, counter, _) =>
      forward(submitted.order.sender, e)
      if (counter.order.sender != submitted.order.sender) {
        forward(counter.order.sender, e)
      }
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
