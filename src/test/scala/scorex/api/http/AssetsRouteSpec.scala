package scorex.api.http

class AssetsRouteSpec extends RouteSpec("/assets/") {
  routePath("balance/{address}/{assetId}") in pending
  routePath("balance/{address}") in pending
  routePath("transfer") in pending
  routePath("issue") in pending
  routePath("reissue") in pending
  routePath("burn") in pending
  routePath("order") in pending
}
