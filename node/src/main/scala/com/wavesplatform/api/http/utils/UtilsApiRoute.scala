package com.wavesplatform.api.http.utils

import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.{PathMatcher1, Route}
import cats.syntax.either.*
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.*
import com.wavesplatform.api.http.ApiError.{CustomValidationError, ScriptCompilerError, ScriptExecutionError, TooBigArrayAllocation}
import com.wavesplatform.api.http.requests.{ScriptWithImportsRequest, byteStrFormat}
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
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.lang.{API, CompileResult, ValidationError}
import com.wavesplatform.serialization.ScriptValuesJson
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.transaction.TxValidationError.{GenericError, InvokeRejectError}
import com.wavesplatform.transaction.smart.script.trace.TraceStep
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
              { cr: CompileResult =>
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
    ) { (address, request, trace, accept) =>
      val scriptInfo = blockchain.accountScript(address).get
      val pk         = scriptInfo.publicKey
      val script     = scriptInfo.script
      val limit      = settings.evaluateScriptComplexityLimit

      val evaluated = (request.value.get("expr"), request.asOpt[UtilsInvocationRequest]) match {
        case (Some(_), Some(_)) if request.fields.size > 1 => Left(ConflictingRequestStructure)
        case (None, None)                                  => Left(WrongJson)
        case (Some(exprRequest), _) =>
          parseCall(exprRequest, script.stdLibVersion).flatMap(expr =>
            UtilsEvaluator.executeExpression(blockchain, script, address, pk, limit)(
              UtilsEvaluator.emptyInvokeScriptLike(address),
              dApp => Right(ContractEvaluator.buildSyntheticCall(dApp, expr, ByteStr(DefaultAddress.bytes), DefaultPublicKey))
            )
          )
        case (None, Some(invocationRequest)) =>
          invocationRequest.toInvocation.flatMap(invocation =>
            UtilsEvaluator.executeExpression(blockchain, script, address, pk, limit)(
              UtilsEvaluator.toInvokeScriptLike(invocation, address),
              ContractEvaluator.buildExprFromInvocation(_, invocation, script.stdLibVersion).bimap(e => GenericError(e.message), _.expr)
            )
          )
      }

      val apiResult = evaluated
        .fold(
          {
            case e: InvokeRejectError        => Json.obj("error" -> ScriptExecutionError.Id, "message" -> e.toStringWithLog(maxTxErrorLogSize))
            case WrongJson                   => ApiError.WrongJson().json
            case ConflictingRequestStructure => ApiError.ConflictingRequestStructure.json
            case e                           => ApiError.fromValidationError(e).json
          },
          { case (result, complexity, log, scriptResult) =>
            val intAsString = accept.exists(_.mediaRanges.exists(CustomJson.acceptsNumbersAsStrings))
            val traceObj    = if (trace) Json.obj(TraceStep.logJson(log)) else Json.obj()
            traceObj ++ Json.obj(
              "result"       -> ScriptValuesJson.serializeValue(result, intAsString),
              "complexity"   -> complexity,
              "stateChanges" -> scriptResult
            )
          }
        )

      complete(apiResult ++ request ++ Json.obj("address" -> address.toString))
    }

  private def parseCall(js: JsReadable, version: StdLibVersion) = {
    val binaryCall = js
      .asOpt[ByteStr]
      .toRight(GenericError("Unable to parse expr bytes"))
      .flatMap(bytes => SerdeV1.deserialize(bytes.arr).bimap(GenericError(_), _._1))

    val textCall = js
      .asOpt[String]
      .toRight(GenericError("Unable to read expr string"))
      .flatMap(UtilsEvaluator.compile(version))

    binaryCall.orElse(textCall)
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

  object WrongJson                   extends ValidationError
  object ConflictingRequestStructure extends ValidationError
}
