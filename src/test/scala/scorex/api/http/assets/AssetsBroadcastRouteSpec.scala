package scorex.api.http.assets

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.RestAPISettings
import de.heikoseeberger.akkahttpplayjson.PlayJsonSupport._
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, JsValue, Json}
import scorex.api.http.{RouteSpec, StateCheckFailed}
import scorex.transaction.{SimpleTransactionModule, Transaction, TransactionGen}


class AssetsBroadcastRouteSpec extends RouteSpec("/assets/broadcast/") with PathMockFactory
  with TransactionGen with PropertyChecks {

  "returns StateCheckFiled when state validation fails" - {
    val stmMock = {
      val m = mock[SimpleTransactionModule]
      (m.onNewOffchainTransaction _).expects(*).onCall { _: Transaction => false } anyNumberOfTimes()
      m
    }

    val route = AssetsBroadcastApiRoute(RestAPISettings.fromConfig(ConfigFactory.parseString("")), stmMock).route

    val vt = Table[String, Gen[_ <: Transaction], (JsValue) => JsValue](
      ("url", "generator", "transform"),
      (routePath("issue"), issueGenerator, identity),
      (routePath("reissue"), reissueGenerator, identity),
      (routePath("burn"), burnGenerator, {
        case o: JsObject => o ++ Json.obj("quantity" -> o.value("amount"))
        case other => other
      }),
      (routePath("transfer"), transferGenerator, {
        case o: JsObject if o.value.contains("feeAsset") =>
          o ++ Json.obj("feeAssetId" -> o.value("feeAsset"), "quantity" -> o.value("amount"))
        case other => other
      })
    )

    forAll(vt) { (url, gen, transform) =>
      url in {
        forAll(gen) { v =>
          Post(url, transform(v.json)) ~> route ~> check {
            status shouldBe StatusCodes.BadRequest
            responseAs[JsObject] shouldEqual StateCheckFailed.json
          }
        }
      }
    }
  }
}
