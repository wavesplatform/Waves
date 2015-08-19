package scorex.app.api.http

import akka.actor.Actor

class HttpServiceActor extends Actor
with AddressHttpService
with BlocksHttpService
with ScorexHttpService
with SeedHttpService
with PeersHttpService
with PaymentHttpService
with WalletHttpService
with TransactionsHttpService {
  override def actorRefFactory = context

  override def receive = runRoute(adressesRouting ~ blocksRouting ~ scorexRouting ~ seedRouting ~
    peersRouting ~ walletRouting ~ transactionsRouting ~ paymentRouting)
}