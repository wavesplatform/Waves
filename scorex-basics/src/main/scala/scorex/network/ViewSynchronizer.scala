package scorex.network

import akka.actor.{Actor, ActorRef}
import scorex.network.message.MessageSpec


/**
  * Synchronizing network & local views of an object, e.g. history(blockchain or blocktree), known peers list,
  * segments dataset subset etc.
  *
  * todo: anti-ddos?
  */
trait ViewSynchronizer extends Actor {

  protected val networkControllerRef: ActorRef

  val messageSpecs: Seq[MessageSpec[_]]

  override final def preStart: Unit = {
    networkControllerRef ! NetworkController.RegisterMessagesHandler(messageSpecs, self)
  }
}
