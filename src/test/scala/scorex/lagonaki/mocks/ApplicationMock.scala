package scorex.lagonaki.mocks

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.server.Route
import scorex.app.Application
import scorex.network.UPnP
import scorex.settings.SettingsMock
import scorex.transaction.{BlockStorage, History}
import scorex.wallet.Wallet

trait ApplicationMock extends Application  {
  override lazy val settings = DefaultTestSettings
  override lazy val upnp: UPnP = ???
  override lazy val peerManager: ActorRef = ???
  override lazy val wallet: Wallet = ???
  override lazy val networkController: ActorRef = ???
  override lazy val blockGenerator: ActorRef = ???
  override lazy val scoreObserver: ActorRef = ???
  override lazy val blockChainSynchronizer: ActorRef = ???
  override lazy val coordinator: ActorRef = ???
  override lazy val historyReplier: ActorRef = ???
  override lazy val combinedRoute: Route = ???
  override lazy val blockStorage: BlockStorage = ???
  override lazy val history: History = ???
  override protected lazy val actorSystem: ActorSystem = ???
/*
  TODO: not needed, remove after test stabilization
  override lazy val applicationName: String = ???
  override def appVersion: ApplicationVersion = ???
  override lazy val consensusModule: ConsensusModule[_] = null
  override lazy val transactionModule: TransactionModule[_] = null
  override lazy val apiRoutes: Seq[ApiRoute] = ???
  override lazy val apiTypes: Seq[_root_.scala.reflect.runtime.universe.Type] = ???
  override lazy protected val additionalMessageSpecs: Seq[MessageSpec[_]] = ???
*/

  object DefaultTestSettings extends SettingsMock
}
