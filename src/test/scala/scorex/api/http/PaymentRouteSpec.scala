package scorex.api.http

import com.wavesplatform.http.RouteSpec

class PaymentRouteSpec extends RouteSpec("/payment") {
  routePath("") in pending
}
