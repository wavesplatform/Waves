package com.wavesplatform.api.http.alias

import akka.http.scaladsl.server.Route
import cats.syntax.either._
import com.wavesplatform.account.Alias
import com.wavesplatform.api.http._
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction._
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.Json

case class AliasApiRoute(settings: RestAPISettings, wallet: Wallet, utxPoolSynchronizer: UtxPoolSynchronizer, time: Time, blockchain: Blockchain)
    extends ApiRoute
    with BroadcastRoute
    with AuthRoute {

  override val route: Route = pathPrefix("alias") {
    addressOfAlias ~ aliasOfAddress ~ deprecatedRoute
  }

  private def deprecatedRoute: Route =
    path("broadcast" / "create") {
      broadcast[SignedCreateAliasV1Request](_.toTx)
    } ~ (path("create") & withAuth) {
      broadcast[CreateAliasV1Request](TransactionFactory.aliasV1(_, wallet, time))
    }

  def addressOfAlias: Route = (get & path("by-alias" / Segment)) { aliasName =>
    complete {
      Alias
        .create(aliasName)
        .flatMap { a =>
          blockchain.resolveAlias(a).bimap(_ => TxValidationError.AliasDoesNotExist(a), addr => Json.obj("address" -> addr.stringRepr))
        }
    }
  }

  def aliasOfAddress: Route = (get & path("by-address" / Segment)) { addressString =>
    complete {
      com.wavesplatform.account.Address
        .fromString(addressString)
        .map(acc => blockchain.aliasesOfAddress(acc).map(_.stringRepr).toVector)
    }
  }
}
