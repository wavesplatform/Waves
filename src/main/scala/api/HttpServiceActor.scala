package api

import akka.actor.Actor

class HttpServiceActor extends Actor
  with AddressHttpService
  with BlocksHttpService
  with ScorexHttpService
 {

  override def actorRefFactory = context
  override def receive = runRoute(adressesRouting ~ blocksRouting ~ scorexRouting)
}