package scorex.api.http

import com.wavesplatform.http.RouteSpec

class NodeRouteSpec extends RouteSpec("/node/") {
  routePath("version") in pending
  routePath("stop") in pending
  routePath("status") in pending
}
