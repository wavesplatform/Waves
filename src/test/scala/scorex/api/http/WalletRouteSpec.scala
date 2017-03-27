package scorex.api.http

import com.wavesplatform.http.RouteSpec

class WalletRouteSpec extends RouteSpec("/wallet/") {
  routePath("seed") in pending
}
