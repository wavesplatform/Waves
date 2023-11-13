package com.wavesplatform.api.http.utils

import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.{PathMatcher1, Route}
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.*
import com.wavesplatform.api.http.ApiError.{CustomValidationError, ScriptCompilerError, TooBigArrayAllocation}
import com.wavesplatform.api.http.requests.ScriptWithImportsRequest
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.crypto
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.{RideV6, SynchronousCalls}
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.Script.ComplexityInfo
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.{API, CompileResult}
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.utils.Time
import monix.execution.Scheduler
import play.api.libs.json.*

import java.security.SecureRandom

case class UtilsApiRoute(
    timeService: Time,
    settings: RestAPISettings,
    maxTxErrorLogSize: Int,
    estimator: () => ScriptEstimator,
    limitedScheduler: Scheduler,
    blockchain: Blockchain
) extends ApiRoute
    with AuthRoute
    with TimeLimitedRoute {

  import UtilsApiRoute.*

  private def seed(length: Int): JsObject = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) // seed mutated here!
    Json.obj("seed" -> Base58.encode(seed))
  }

  private def extraFee(verifierComplexity: Long): Long =
    if (verifierComplexity <= ContractLimits.FreeVerifierComplexity && blockchain.isFeatureActivated(SynchronousCalls))
      0
    else
      FeeValidation.ScriptExtraFee

  override val route: Route = pathPrefix("utils") {
    decompile ~ compileCode ~ compileWithImports ~ estimate ~ time ~ seedRoute ~ length ~ hashFast ~ hashSecure ~ transactionSerialize ~ evaluate
  }

  def decompile: Route = path("script" / "decompile") {
    import play.api.libs.json.Json.toJsFieldJsValueWrapper

    (post & entity(as[String])) { code =>
      Script.fromBase64String(code.trim) match {
        case Left(err) => complete(err)
        case Right(script) =>
          executeLimited(Script.decompile(script)) { case (scriptText, meta) =>
            val directives: List[(String, JsValue)] = meta.map { case (k, v) =>
              (
                k,
                v match {
                  case n: Number => JsNumber(BigDecimal(n.toString))
                  case s         => JsString(s.toString)
                }
              )
            }
            val result  = directives ::: "script" -> JsString(scriptText) :: Nil
            val wrapped = result.map { case (k, v) => (k, toJsFieldJsValueWrapper(v)) }
            complete(
              Json.obj(wrapped*)
            )
          }
      }
    }
  }

  private def defaultStdlibVersion(): StdLibVersion = {
    val v5Activated = blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls)
    if (v5Activated)
      V5
    else if (blockchain.isFeatureActivated(BlockchainFeatures.Ride4DApps))
      V4
    else
      StdLibVersion.VersionDic.default
  }

  def compileCode: Route = path("script" / "compileCode") {
    (post & entity(as[String]) & parameter("compact".as[Boolean] ? false)) { (code, compact) =>
      executeLimited(
        API.compile(
          code,
          estimator(),
          compact,
          defaultStdLib = defaultStdlibVersion(),
          allowFreeCall = blockchain.isFeatureActivated(BlockchainFeatures.ContinuationTransaction)
        )
      )(
        _.fold(
          e => complete(ScriptCompilerError(e)),
          { cr =>
            val v5Activated = blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls)
            val extraFee =
              if (cr.verifierComplexity <= ContractLimits.FreeVerifierComplexity && v5Activated)
                0
              else
                FeeValidation.ScriptExtraFee

            complete(
              Json.obj(
                "script"               -> ByteStr(cr.bytes).base64,
                "complexity"           -> cr.maxComplexity,
                "verifierComplexity"   -> cr.verifierComplexity,
                "callableComplexities" -> cr.callableComplexities,
                "extraFee"             -> extraFee
              )
            )
          }
        )
      )
    }
  }

  def compileWithImports: Route = path("script" / "compileWithImports") {
    import ScriptWithImportsRequest.*
    (post & entity(as[ScriptWithImportsRequest])) { req =>
      executeLimited(
        API.compile(
          req.script,
          estimator(),
          needCompaction = false,
          removeUnusedCode = false,
          req.imports,
          defaultStdLib = defaultStdlibVersion()
        )
      ) { result =>
        complete(
          result
            .fold(
              e => ScriptCompilerError(e),
              { (cr: CompileResult) =>
                Json.obj(
                  "script"     -> ByteStr(cr.bytes).base64,
                  "complexity" -> cr.verifierComplexity,
                  "extraFee"   -> FeeValidation.ScriptExtraFee
                )
              }
            )
        )
      }
    }
  }

  def estimate: Route = path("script" / "estimate") {
    (post & entity(as[String])) { code =>
      executeLimited(
        Script
          .fromBase64String(code)
          .left
          .map(_.m)
          .flatMap { script =>
            Script
              .complexityInfo(
                script,
                estimator(),
                fixEstimateOfVerifier = blockchain.isFeatureActivated(RideV6),
                useContractVerifierLimit = false,
                withCombinedContext = true
              )
              .map((script, _))
          }
      ) { result =>
        complete(
          result
            .fold(
              e => ScriptCompilerError(e),
              { case (script, ComplexityInfo(verifierComplexity, callableComplexities, maxComplexity)) =>
                Json.obj(
                  "script"               -> code,
                  "scriptText"           -> script.expr.toString, // [WAIT] Script.decompile(script),
                  "complexity"           -> maxComplexity,
                  "verifierComplexity"   -> verifierComplexity,
                  "callableComplexities" -> callableComplexities,
                  "extraFee"             -> extraFee(verifierComplexity)
                )
              }
            )
        )
      }
    }
  }

  def time: Route = (path("time") & get) {
    complete(Json.obj("system" -> System.currentTimeMillis(), "NTP" -> timeService.correctedTime()))
  }

  def seedRoute: Route = (path("seed") & get) {
    complete(seed(DefaultSeedSize))
  }

  def length: Route = (path("seed" / IntNumber) & get) { length =>
    if (length <= MaxSeedSize) complete(seed(length))
    else complete(TooBigArrayAllocation)
  }

  def hashSecure: Route = (path("hash" / "secure") & post) {
    entity(as[String]) { message =>
      complete(Json.obj("message" -> message, "hash" -> Base58.encode(crypto.secureHash(message))))
    }
  }

  def hashFast: Route = (path("hash" / "fast") & post) {
    entity(as[String]) { message =>
      complete(Json.obj("message" -> message, "hash" -> Base58.encode(crypto.fastHash(message))))
    }
  }

  def transactionSerialize: Route =
    path("transactionSerialize")(jsonPost[JsObject] { jsv =>
      parseOrCreateTransaction(jsv)(tx => Json.obj("bytes" -> tx.bodyBytes().map(_.toInt & 0xff)))
    })

  def evaluate: Route =
    (
      path("script" / "evaluate" / ScriptedAddress)
        & jsonPostD[JsObject]
        & parameter("trace".as[Boolean] ? false)
        & optionalHeaderValueByType(Accept)
    ) { (address, request, enableTraces, accept) =>
      val apiResult = UtilsEvaluator.evaluate(
        blockchain,
        address,
        request,
        UtilsEvaluator.EvaluateOptions(
          evaluateScriptComplexityLimit = settings.evaluateScriptComplexityLimit,
          maxTxErrorLogSize = maxTxErrorLogSize,
          enableTraces = enableTraces,
          intAsString = accept.exists(_.mediaRanges.exists(CustomJson.acceptsNumbersAsStrings))
        )
      )
      complete(apiResult ++ request ++ Json.obj("address" -> address.toString))
    }

  private[this] val ScriptedAddress: PathMatcher1[Address] = AddrSegment.map {
    case address: Address if blockchain.hasAccountScript(address) => address
    case other                                                    => throw ApiException(CustomValidationError(s"Address $other is not dApp"))
  }
}

object UtilsApiRoute {
  val MaxSeedSize                 = 1024
  val DefaultSeedSize             = 32
  val DefaultPublicKey: PublicKey = PublicKey(ByteStr(new Array[Byte](32)))
  val DefaultAddress: Address     = DefaultPublicKey.toAddress
}
