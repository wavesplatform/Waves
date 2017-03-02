package scorex.api.http

import java.io.File

import akka.http.scaladsl.model.headers.RawHeader
import com.typesafe.config.ConfigFactory
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.RestAPISettings
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, Json}
import scorex.api.http.assets.AssetsApiRoute
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction._
import scorex.wallet.Wallet

class AssetsRouteSpec extends RouteSpec("/assets/") with RequestGen with PathMockFactory with PropertyChecks {

  import AssetsRouteSpec._

  private val wallet = {
    val file = File.createTempFile("wallet", ".dat")
    file.deleteOnExit()
    new Wallet(Some(file.getCanonicalPath), "123", None)
  }

  private def txsOperationsMock(expectedError: ValidationError) = {
    val m = mock[TransactionOperations]
    (m.transferAsset _).expects(*, *).onCall { (_, _) => Left(expectedError) }.anyNumberOfTimes()
    (m.issueAsset _).expects(*, *).onCall { (_, _) => Left(expectedError) }.anyNumberOfTimes()
    (m.reissueAsset _).expects(*, *).onCall { (_, _) => Left(expectedError) }.anyNumberOfTimes()
    (m.burnAsset _).expects(*, *).onCall { (_, _) => Left(expectedError) }.anyNumberOfTimes()
    m
  }

  private val txMock = new Transaction {
    override val assetFee: (Option[AssetId], Long) = (None, 0)
    override val timestamp: Long = 0
    override val id: Array[Byte] = Array()
    override def bytes: Array[Byte] = ???
    override def json: JsObject = Json.obj("k" -> "v")
    override val transactionType = TransactionType.TransferTransaction
  }

  private val errorGen: Gen[ValidationError] = Gen.oneOf(
    ValidationError.InvalidAddress,
    ValidationError.NegativeAmount,
    ValidationError.InsufficientFee,
    ValidationError.TooBigArray,
    ValidationError.InvalidSignature,
    ValidationError.InvalidName,
    ValidationError.OverflowError,
    ValidationError.ToSelf,
    ValidationError.MissingSenderPrivateKey,
    ValidationError.TransactionParameterValidationError("custom.error"),
    ValidationError.TransactionValidationError(txMock, "state.validation.error")
  )

  routePath("balance/{address}/{assetId}") in pending
  routePath("balance/{address}") in pending


  for ((path, gen) <- Seq(
    "transfer" -> transferReq.map(v => Json.toJson(v)),
    "issue" -> issueReq.map(v => Json.toJson(v)),
    "reissue" -> reissueReq.map(v => Json.toJson(v)),
    "burn" -> burnReq.map(v => Json.toJson(v))
  )) {
    val currentPath = routePath(path)
    currentPath in {
      forAll(errorGen) { e =>
        val route = AssetsApiRoute(settings, wallet, mock[State], txsOperationsMock(e)).route

        forAll(gen) { tr =>
          val p = Post(currentPath, tr)

          p ~> route should produce(ApiKeyNotValid)
          p.addHeader(RawHeader("api_key", apiKey)) ~> route should produce(ApiError.fromValidationError(e))
        }
      }
    }
  }

  routePath("order") in pending
}

object AssetsRouteSpec {
  private[AssetsRouteSpec] val apiKey = "test"

  private[AssetsRouteSpec] val settings = {
    val keyHash = Base58.encode(SecureCryptographicHash(apiKey))
    RestAPISettings.fromConfig(
      ConfigFactory
        .parseString(s"waves.rest-api.api-key-hash = $keyHash")
        .withFallback(ConfigFactory.load()))
  }
}
