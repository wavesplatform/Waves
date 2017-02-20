package scorex.api.http

import java.io.File
import akka.http.scaladsl.model.headers.RawHeader
import com.typesafe.config.ConfigFactory
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.RestAPISettings
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.{TransactionOperations, ValidationError}
import scorex.wallet.Wallet

class AssetsRouteSpec extends RouteSpec("/assets/") with RequestGen with PathMockFactory with PropertyChecks {
  import AssetsRouteSpec._

  private val wallet = {
    val file = File.createTempFile("wallet", ".dat")
    file.deleteOnExit()
    new Wallet(Some(file.getCanonicalPath), "123", None)
  }

  private def mkMock(expectedError: ValidationError) = {
    val m = mock[TransactionOperations]
    (m.transferAsset _).expects(*, *).onCall { (_, _) => Left(expectedError) }.anyNumberOfTimes()
    (m.issueAsset    _).expects(*, *).onCall { (_, _) => Left(expectedError) }.anyNumberOfTimes()
    (m.reissueAsset  _).expects(*, *).onCall { (_, _) => Left(expectedError) }.anyNumberOfTimes()
    (m.burnAsset     _).expects(*, *).onCall { (_, _) => Left(expectedError) }.anyNumberOfTimes()
    m
  }

  private val errorGen: Gen[ValidationError] = Gen.oneOf(
    ValidationError.InvalidAddress,
    ValidationError.NegativeAmount,
    ValidationError.InsufficientFee,
    ValidationError.NoBalance,
    ValidationError.TooBigArray,
    ValidationError.InvalidSignature,
    ValidationError.InvalidName,
    ValidationError.StateCheckFailed,
    ValidationError.OverflowError,
    ValidationError.CustomValidationError("custom.error"),
    ValidationError.StateValidationError("state.validation.error")
  )

  routePath("balance/{address}/{assetId}") in pending
  routePath("balance/{address}") in pending


  for ((path, gen) <- Seq(
    "transfer" -> transferReq.map(v => Json.toJson(v)),
    "issue" -> issueReq.map(v => Json.toJson(v)),
    "reissue"-> reissueReq.map(v => Json.toJson(v)),
    "burn" -> burnReq.map(v => Json.toJson(v))
  )) {
    val currentPath = routePath(path)
    currentPath in {
      forAll(errorGen) { e =>
        val route = AssetsApiRoute(settings, wallet, mock[StoredState], mkMock(e)).route

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
