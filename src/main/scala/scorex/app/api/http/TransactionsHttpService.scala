package scorex.app.api.http

import play.api.libs.json.Json
import scorex.app.LagonakiApplication
import scorex.crypto.Base58
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import spray.routing.HttpService

import scala.util.Try


trait TransactionsHttpService extends HttpService with CommonApiFunctions {

  val application:LagonakiApplication

  lazy val transactionsRouting =
    pathPrefix("transactions") {
      path("signature" / Segment) { case signature =>
        get {
          complete {
            Try(Base58.decode(signature)).map { signatureBytes =>
              ??? //todo: implement?
            }.getOrElse(ApiError.json(ApiError.InvalidSignature)).toString()
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
            val txJsons = application.storedState.accountTransactions(address).map(_.json())
            Json.arr(txJsons).toString()
          }
        }
      } ~ path("address" / Segment / "limit" / IntNumber) { case (address, limit) =>
        get {
          complete {
            val txJsons = application.storedState.accountTransactions(address)
              .takeRight(limit)
              .map(_.json())
            Json.arr(txJsons).toString()
          }
        }
      }
    }
}
