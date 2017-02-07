package scorex.api.http

class AddressRouteSpec extends RouteSpec("/addresses/") {
  routePath("") in pending
  routePath("{address}") in pending
  routePath("sign/{address}") in pending
  routePath("signText/{address}") in pending
  routePath("verify/{address}") in pending
  routePath("verifyText/{address}") in pending
  routePath("balance/{address}") in pending
  routePath("seed/{address}") in pending
  routePath("validate/{address}") in pending
  routePath("seq/{from}/{to}") in pending
  routePath("publicKey/{publicKey}") in pending
}
