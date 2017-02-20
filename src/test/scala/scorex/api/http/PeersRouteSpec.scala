package scorex.api.http

class PeersRouteSpec extends RouteSpec("/peers/") {
  routePath("connected") in pending
  routePath("all") in pending
  routePath("connect") in pending
  routePath("blacklisted") in pending
}
