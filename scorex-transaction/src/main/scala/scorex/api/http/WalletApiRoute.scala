package scorex.api.http

import akka.actor.ActorRefFactory
import play.api.libs.json.Json
import scorex.crypto.Base58
import scorex.transaction.state.wallet.Wallet


case class WalletApiRoute(implicit wallet: Wallet, implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  override lazy val route = {
    pathPrefix("wallet") {
      path("") {
        get {
          complete(Json.obj("exists" -> wallet.exists()).toString())
        }
      } ~ path("seed") {
        get {
          complete {
            lazy val seedJs = Json.obj("seed" -> Base58.encode(wallet.exportSeed()))
            walletNotExists().getOrElse(seedJs).toString()
          }
        }
      }
    }
  }
}
