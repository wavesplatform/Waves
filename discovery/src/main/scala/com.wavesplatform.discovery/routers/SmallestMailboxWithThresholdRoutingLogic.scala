package com.wavesplatform.discovery.routers

import akka.actor.ActorRef
import akka.routing.{Routee, SmallestMailboxRoutingLogic}

import scala.collection.immutable

case class SmallestMailboxWithThresholdRoutingLogic(mailboxThreshold: Int) extends SmallestMailboxRoutingLogic {
  override def select(message: Any, routees: immutable.IndexedSeq[Routee]): Routee = {

    if (routees.exists(numberOfMessages(_) < mailboxThreshold)) {
      super.select(message, routees)
    } else { (_: Any, _: ActorRef) =>
      ()
    }
  }
}
