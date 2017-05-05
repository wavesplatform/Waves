package scorex.network

import akka.actor.{Actor, ActorRef}
import scorex.network.message.MessageSpec

trait ViewSynchronizer extends Actor {

  protected val networkControllerRef: ActorRef

  protected val messageSpecs: Seq[MessageSpec[_]]

  override final def preStart: Unit = {
    networkControllerRef ! NetworkController.RegisterMessagesHandler(messageSpecs, self)
  }
}
