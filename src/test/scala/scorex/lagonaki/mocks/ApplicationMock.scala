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
  override lazy val blockchainSynchronizer: ActorRef = ???
  override lazy val coordinator: ActorRef = ???
  override lazy val historyReplier: ActorRef = ???
  override lazy val combinedRoute: Route = ???
  override lazy val blockStorage: BlockStorage = ???
  override lazy val history: History = ???
  override protected lazy val actorSystem: ActorSystem = ???

  object DefaultTestSettings extends SettingsMock
}
