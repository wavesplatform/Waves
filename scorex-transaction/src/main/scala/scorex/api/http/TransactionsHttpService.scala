package scorex.api.http

import play.api.libs.json.Json
import scorex.crypto.Base58
import scorex.transaction.state.LagonakiState
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import spray.routing.HttpService._

import scala.util.Try


case class TransactionsHttpService(state:LagonakiState) extends ApiRoute with CommonApiFunctions {
  override lazy val route =
    pathPrefix("transactions") {
      path("signature" / Segment) { case signature =>
        get {
          complete {
            Try(Base58.decode(signature)).map { signatureBytes =>
              ??? //todo: implement?
            }.getOrElse(InvalidSignature.json).toString()
          }
        }
      } ~ path("unconfirmed") {
        get {
          complete {
            Json.arr(UnconfirmedTransactionsDatabaseImpl.all().map(_.json())).toString()
          }
        }
      } ~ path("address" / Segment) { case address =>
        get {
          complete {
            val txJsons = state.accountTransactions(address).map(_.json())
            Json.arr(txJsons).toString()
          }
        }
      } ~ path("address" / Segment / "limit" / IntNumber) { case (address, limit) =>
        get {
          complete {
            val txJsons = state.accountTransactions(address)
              .takeRight(limit)
              .map(_.json())
            Json.arr(txJsons).toString()
          }
        }
      }
    }
}
