package scorex.api.http.assets

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.RestAPISettings
import de.heikoseeberger.akkahttpplayjson.PlayJsonSupport._
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import play.api.libs.json.{JsObject, JsValue, Json}
import scorex.api.http.StateCheckFailed
import scorex.transaction.{SimpleTransactionModule, Transaction, TransactionGen}


class AssetsBroadcastRouteSpec extends FreeSpec with Matchers with ScalatestRouteTest with PathMockFactory
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
      ("/assets/broadcast/issue", issueGenerator, identity),
      ("/assets/broadcast/reissue", reissueGenerator, identity),
      ("/assets/broadcast/burn", burnGenerator, {
        case o: JsObject => o ++ Json.obj("quantity" -> o.value("amount"))
        case other => other
      }),
      ("/assets/broadcast/transfer", transferGenerator, {
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
