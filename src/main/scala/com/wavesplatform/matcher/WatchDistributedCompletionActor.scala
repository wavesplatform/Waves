package com.wavesplatform.matcher

import akka.actor.{Actor, ActorRef, Props, Terminated}

class WatchDistributedCompletionActor(workers: Set[ActorRef], completionReceiver: ActorRef, startWorkCommand: Any, workCompleted: Any) extends Actor {
  workers.foreach { x =>
    context.watch(x)
    x ! startWorkCommand
  }

  override def receive: Receive = state(workers)

  private def state(rest: Set[ActorRef]): Receive = {
    case `workCompleted` =>
      switchTo(rest - sender())
      context.unwatch(sender())

    case Terminated(ref) =>
      switchTo(rest - ref)
  }

  private def switchTo(updatedRest: Set[ActorRef]): Unit =
    if (updatedRest.isEmpty) {
      completionReceiver ! workCompleted
      context.stop(self)
    } else context.become(state(updatedRest))
}

object WatchDistributedCompletionActor {
  def props(workers: Set[ActorRef], completionReceiver: ActorRef, startWorkCommand: Any, workCompleted: Any): Props =
    Props(new WatchDistributedCompletionActor(workers, completionReceiver, startWorkCommand, workCompleted))
}
