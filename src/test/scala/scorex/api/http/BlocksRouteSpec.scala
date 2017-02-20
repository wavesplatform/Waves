package scorex.api.http

class BlocksRouteSpec extends RouteSpec("/blocks/") {
  routePath("address/{address}/{from}/{to}") in pending
  routePath("child/{signature}") in pending
  routePath("delay/{signature}/{blockNum}") in pending
  routePath("height/{signature}") in pending
  routePath("height") in pending
  routePath("at/{height}") in pending
  routePath("seq/{from}/{to}") in pending
  routePath("last") in pending
  routePath("first") in pending
  routePath("signature/{signature}") in pending
  routePath("checkpoint") in pending
}
