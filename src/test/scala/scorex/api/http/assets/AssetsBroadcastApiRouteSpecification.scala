package scorex.api.http.assets

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.RestAPISettings
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FreeSpec, Matchers}
import scorex.transaction.SimpleTransactionModule


class AssetsBroadcastApiRouteSpecification extends FreeSpec with Matchers with ScalatestRouteTest with PathMockFactory {
  "/assets/broadcast/issue" ignore {
    val stmMock = mock[SimpleTransactionModule]
    val abar = AssetsBroadcastApiRoute(RestAPISettings.fromConfig(ConfigFactory.parseString("")), stmMock)
    val json =
      """{
        |"senderPublicKey":"4c4nAckNxsuafXcg4abFPJ6wBZB6PD7KVNkD1wbVxnxZ",
        |"fee":10000000,
        |"signature":"123",
        |"name":"",
        |"quantity":0,
        |"reissuable":false,
        |"decimals":10,
        |"description":"",
        |"timestamp":1
        |}""".stripMargin
    Post("/assets/broadcast/issue", HttpEntity(ContentTypes.`application/json`, json)) ~> abar.route ~> check {
      println(response)
      status shouldBe StatusCodes.OK
    }
  }
}
