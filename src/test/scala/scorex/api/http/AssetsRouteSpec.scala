package scorex.api.http

import java.io.File
import akka.http.scaladsl.model.headers.RawHeader
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen._
import play.api.libs.json.Json
import scorex.wallet.Wallet
import scorex.settings.Settings
import scorex.transaction.{SimpleTransactionModule, TransactionGen}
import scorex.transaction.state.wallet.TransferRequest
import de.heikoseeberger.akkahttpplayjson.PlayJsonSupport._
import scorex.transaction.state.database.blockchain.StoredState

class AssetsRouteSpec extends RouteSpec("/assets/") with RequestGen with PathMockFactory with PropertyChecks {
  import AssetsRouteSpec._

  private val wallet = {
    val file = File.createTempFile("wallet", ".dat")
    file.deleteOnExit()
    new Wallet(Some(file), "123", None)
  }

  routePath("balance/{address}/{assetId}") in pending
  routePath("balance/{address}") in pending
  routePath("transfer") in {

    val route = AssetsApiRoute(settings, wallet, mock[StoredState], mock[SimpleTransactionModule]).route
    def posting(r: TransferRequest, apiKey: Option[String] = Some(AssetsRouteSpec.apiKey)) =
      apiKey.fold(Post(routePath("transfer"), Json.toJson(r))) { apiKeyValue =>
        Post(routePath("transfer"), Json.toJson(r)).addHeader(RawHeader("apiKey", apiKeyValue))
      } ~> route

    forAll(transferReq) { tr =>
      forAll(nonPositiveLong) { q => posting(tr.copy(amount = q)) should produce (NegativeAmount) }
      forAll(invalidBase58) { pk => posting(tr.copy(sender = pk)) should produce (InvalidAddress) }
      forAll(invalidBase58) { pk => posting(tr.copy(recipient = pk)) should produce (InvalidAddress) }
      forAll(invalidBase58) { a => posting(tr.copy(assetId = Some(a))) should produce (CustomValidationError("invalid.assetId")) }
      forAll(invalidBase58) { a => posting(tr.copy(feeAssetId = Some(a))) should produce (CustomValidationError("invalid.feeAssetId")) }
      forAll(longAttachment) { a => posting(tr.copy(attachment = Some(a))) should produce (TooBigArrayAllocation) }
      forAll(posNum[Long]) { quantity => posting(tr.copy(amount = quantity, fee = Long.MaxValue)) should produce (OverflowError) }
      forAll(nonPositiveLong) { fee => posting(tr.copy(fee = fee)) should produce (InsufficientFee) }
    }

  }

  routePath("issue") in {
    forAll(issueReq) { ir =>

    }
  }

  routePath("reissue") in {
    forAll(reissueReq) { rr =>

    }
  }

  routePath("burn") in {
    forAll(burnReq) { br =>

    }
  }

  routePath("order") in pending
}

object AssetsRouteSpec {
  private[AssetsRouteSpec] val apiKey = ""
  private[AssetsRouteSpec] val settings = new Settings {
    override def settingsJSON = Json.obj("apiKeyHash" -> apiKey)
  }

  private[AssetsRouteSpec] object g extends TransactionGen {

  }
}
