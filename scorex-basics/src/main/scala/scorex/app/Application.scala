package scorex.app

import akka.actor.{Props, ActorSystem}
import scorex.api.http.{ApiRoute, CompositeHttpServiceActor}
import scorex.consensus.ConsensusModule
import scorex.settings.Settings
import scorex.transaction.TransactionModule

trait Application {
  val applicationName:String

  implicit val settings: Settings

  implicit val consensusModule : ConsensusModule[_]
  implicit val transactionModule: TransactionModule[_]

  val apiRoutes: Seq[ApiRoute]

  protected implicit lazy val actorSystem = ActorSystem("lagonaki")
  lazy val apiActor = actorSystem.actorOf(Props(classOf[CompositeHttpServiceActor], apiRoutes), "api")
}
