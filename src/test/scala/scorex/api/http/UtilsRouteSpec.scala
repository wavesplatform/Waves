package scorex.api.http

import com.wavesplatform.http.RouteSpec

class UtilsRouteSpec extends RouteSpec("/utils/") {
  routePath("seed") in pending
  routePath("seed/{length}") in pending
  routePath("hash/secure") in pending
  routePath("hash/fast") in pending
}
