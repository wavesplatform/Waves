package scorex.api.http

class TransactionsRouteSpec extends RouteSpec("/transactions/") {
  routePath("address/{address}/limit/{limit}") in pending
  routePath("info/{signature}") in pending
  routePath("unconfirmed") in pending
}
