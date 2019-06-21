package com.wavesplatform.matcher

import akka.actor.{Actor, ActorRef, Props, SupervisorStrategy, Terminated}
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.matcher.history.HistoryRouter
import com.wavesplatform.matcher.model.Events
import com.wavesplatform.matcher.settings.MatcherSettings
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.collection.mutable

class AddressDirectory(spendableBalanceChanged: Observable[(Address, Asset)],
                       settings: MatcherSettings,
                       addressActorProps: (Address, Boolean) => Props,
                       historyRouter: Option[ActorRef])
    extends Actor
    with ScorexLogging {
  import AddressDirectory._
  import context._

  private var startSchedules: Boolean = false
  private[this] val children          = mutable.AnyRefMap.empty[Address, ActorRef]

  spendableBalanceChanged
    .filter(x => children.contains(x._1))
    .bufferTimed(settings.balanceWatchingBufferInterval)
    .filter(_.nonEmpty)
    .foreach { changes =>
      val acc = mutable.Map.empty[Address, Set[Asset]]

      changes.foreach { case (addr, changed)   => acc.update(addr, acc.getOrElse(addr, Set.empty) + changed) }
      acc.foreach { case (addr, changedAssets) => children.get(addr).foreach(_ ! AddressActor.BalanceUpdated(changedAssets)) }

    }(Scheduler(context.dispatcher))

  override def supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private def createAddressActor(address: Address): ActorRef = {
    log.debug(s"Creating address actor for $address")
    watch(actorOf(addressActorProps(address, startSchedules), address.toString))
  }

  private def forward(address: Address, msg: Any): Unit = {
    val handler = children.getOrElseUpdate(address, createAddressActor(address))
    handler.forward(msg)
  }

  import HistoryRouter._

  override def receive: Receive = {

    case Envelope(address, cmd) =>
      forward(address, cmd)

    case e @ Events.OrderAdded(lo, timestamp) =>
      forward(lo.order.sender, e)
      historyRouter foreach { _ ! SaveOrder(lo, timestamp) }

    case e @ Events.OrderExecuted(submitted, counter, timestamp) =>
      forward(submitted.order.sender, e)
      if (counter.order.sender != submitted.order.sender) {
        forward(counter.order.sender, e)
      }
      if (e.submittedRemainingAmount == 0) historyRouter foreach { _ ! SaveOrder(submitted, timestamp) } // handle the case when the order is filled right after placing
      historyRouter foreach { _ ! SaveEvent(e) }

    case e @ Events.OrderCanceled(lo, _, _) =>
      forward(lo.order.sender, e)
      historyRouter foreach { _ ! SaveEvent(e) }

    case StartSchedules =>
      if (!startSchedules) {
        startSchedules = true
        context.children.foreach(_ ! StartSchedules)
      }

    case Terminated(child) =>
      val addressString = child.path.name
      val address       = Address.fromString(addressString).explicitGet()
      children.remove(address)
      log.warn(s"Address handler for $addressString terminated")
  }
}

object AddressDirectory {
  case class Envelope(address: Address, cmd: AddressActor.Command)
  case object StartSchedules
}
