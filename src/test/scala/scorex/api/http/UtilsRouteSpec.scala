package scorex.api.http

class UtilsRouteSpec extends RouteSpec("/utils/") {
  routePath("seed") in pending
  routePath("seed/{length}") in pending
  routePath("hash/secure") in pending
  routePath("hash/fast") in pending
}
