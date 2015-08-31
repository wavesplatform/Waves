package scorex.app.api.http

import akka.actor.Actor
import scorex.app.LagonakiApplication

class HttpServiceActor(override val application:LagonakiApplication) extends Actor
  with AddressHttpService
  with BlocksHttpService
  with ScorexHttpService
  with SeedHttpService
  with PeersHttpService
  with PaymentHttpService
  with WalletHttpService
  with TransactionsHttpService {

  override lazy val wallet = application.wallet

  override def actorRefFactory = context

  override def receive = runRoute(adressesRouting ~ blocksRouting ~ scorexRouting ~ seedRouting ~
    peersRouting ~ walletRouting ~ transactionsRouting ~ paymentRouting)
}