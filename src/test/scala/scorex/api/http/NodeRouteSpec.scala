package scorex.api.http

class NodeRouteSpec extends RouteSpec("/node/") {
  routePath("version") in pending
  routePath("stop") in pending
  routePath("status") in pending
}
