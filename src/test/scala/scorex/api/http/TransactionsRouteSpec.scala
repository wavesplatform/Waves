package scorex.api.http

import com.wavesplatform.http.RouteSpec

class TransactionsRouteSpec extends RouteSpec("/transactions/") {
  routePath("address/{address}/limit/{limit}") in pending
  routePath("info/{signature}") in pending
  routePath("unconfirmed") in pending
}
