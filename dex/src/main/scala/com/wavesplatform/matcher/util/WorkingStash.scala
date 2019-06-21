package com.wavesplatform.matcher.util

import akka.actor.{Actor, ActorContext, ActorRef}

import scala.collection.immutable.Queue

// Because the Akka's one doesn't work during actor start, e.g.: https://stackoverflow.com/questions/45305757/akka-unstashall-not-replaying-the-messages
trait WorkingStash {
  this: Actor =>

  private var stashedMessages = Queue.empty[(ActorRef, Any)]

  def stash(message: Any)(implicit actorContext: ActorContext): Unit = stashedMessages = stashedMessages.enqueue(actorContext.sender() -> message)

  def unstashAll(): Unit = {
    stashedMessages.foreach { case (sender, message) => self.tell(message, sender) }
    stashedMessages = Queue.empty
  }
}
