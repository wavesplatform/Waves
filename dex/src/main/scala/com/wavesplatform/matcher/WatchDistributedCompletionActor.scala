package com.wavesplatform.matcher

import akka.actor.{Actor, ActorRef, Props}

class WatchDistributedCompletionActor(workers: Set[ActorRef], completionReceiver: ActorRef, startWorkCommand: Any, workCompleted: Any) extends Actor {
  workers.foreach(_ ! startWorkCommand)

  override def receive: Receive = state(workers)

  private def state(rest: Set[ActorRef]): Receive = {
    case `workCompleted` =>
      val updatedRest = rest - sender()
      if (updatedRest.isEmpty) {
        completionReceiver ! workCompleted
        context.stop(self)
      } else context.become(state(updatedRest))
  }
}

object WatchDistributedCompletionActor {
  def props(workers: Set[ActorRef], completionReceiver: ActorRef, startWorkCommand: Any, workCompleted: Any): Props =
    Props(new WatchDistributedCompletionActor(workers, completionReceiver, startWorkCommand, workCompleted))
}
