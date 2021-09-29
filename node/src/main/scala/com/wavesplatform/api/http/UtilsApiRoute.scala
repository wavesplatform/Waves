package com.wavesplatform.api.http

import java.security.SecureRandom

import akka.http.scaladsl.server.{PathMatcher1, Route}
import cats.syntax.either._
import cats.syntax.semigroup._
import com.wavesplatform.account.{Address, AddressOrAlias, AddressScheme, PublicKey}
import com.wavesplatform.api.http.ApiError.{CustomValidationError, ScriptCompilerError, TooBigArrayAllocation}
import com.wavesplatform.api.http.requests.{ScriptWithImportsRequest, byteStrFormat}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.crypto
import com.wavesplatform.crypto.KeyLength
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.{RideV6, SynchronousCalls}
import com.wavesplatform.features.RideVersionProvider.RideVersionBlockchainExt
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.Script.ComplexityInfo
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, EvaluatorV2}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader, Serde}
import com.wavesplatform.lang.{Global, ValidationError}
import com.wavesplatform.serialization.ScriptValuesJson
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionLike
import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.TxValidationError.{GenericError, ScriptExecutionError}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{BlockchainContext, DAppEnvironment, InvokeScriptTransaction}
import com.wavesplatform.utils.Time
import monix.eval.Coeval
import monix.execution.Scheduler
import play.api.libs.json._
import shapeless.Coproduct
case class UtilsApiRoute(
    timeService: Time,
    settings: RestAPISettings,
    estimator: () => ScriptEstimator,
    limitedScheduler: Scheduler,
    blockchain: Blockchain
) extends ApiRoute
    with AuthRoute
    with TimeLimitedRoute {

  import UtilsApiRoute._

  private def seed(length: Int): JsObject = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Json.obj("seed" -> Base58.encode(seed))
  }

  private def extraFee(verifierComplexity: Long): Long =
    if (verifierComplexity <= ContractLimits.FreeVerifierComplexity && blockchain.isFeatureActivated(SynchronousCalls))
      0
    else
      FeeValidation.ScriptExtraFee

  private def checkInvokeExpression[A](result: Either[String, (Script, A)]): Either[String, (Script, A)] =
    result
      .filterOrElse(
        {
          case (e: ExprScript, _) => !e.isFreeCall || blockchain.isFeatureActivated(RideV6)
          case _                  => true
        },
        "Invoke Expression Transaction is not activated yet"
      )

  override val route: Route = pathPrefix("utils") {
    decompile ~ compile ~ compileCode ~ compileWithImports ~ estimate ~ time ~ seedRoute ~ length ~ hashFast ~ hashSecure ~ transactionSerialize ~ evaluate
  }

  def decompile: Route = path("script" / "decompile") {
    import play.api.libs.json.Json.toJsFieldJsValueWrapper

    (post & entity(as[String])) { code =>
      Script.fromBase64String(code.trim) match {
        case Left(err) => complete(err)
        case Right(script) =>
          executeLimited(Script.decompile(script)) {
            case (scriptText, meta) =>
              val directives: List[(String, JsValue)] = meta.map {
                case (k, v) =>
                  (k, v match {
                    case n: Number => JsNumber(BigDecimal(n.toString))
                    case s         => JsString(s.toString)
                  })
              }
              val result  = directives ::: "script" -> JsString(scriptText) :: Nil
              val wrapped = result.map { case (k, v) => (k, toJsFieldJsValueWrapper(v)) }
              complete(
                Json.obj(wrapped: _*)
              )
          }
      }
    }
  }

  // Deprecated
  def compile: Route = path("script" / "compile") {
    (post & entity(as[String])) { code =>
      parameter("assetScript".as[Boolean] ? false) { isAssetScript =>
        executeLimited(ScriptCompiler(code, isAssetScript, estimator())) { result =>
          complete(
            result.fold(
              e => ScriptCompilerError(e), {
                case (script, complexity) =>
                  Json.obj(
                    "script"     -> script.bytes().base64,
                    "complexity" -> complexity,
                    "extraFee"   -> FeeValidation.ScriptExtraFee
                  )
              }
            )
          )
        }
      }
    }
  }

  def compileCode: Route = path("script" / "compileCode") {
    (post & entity(as[String])) { code =>
      executeLimited(ScriptCompiler.compileAndEstimateCallables(code, estimator(), defaultStdLib = blockchain.actualRideVersion)) { result =>
        complete(
          checkInvokeExpression(result)
            .fold(
              e => ScriptCompilerError(e), {
                case (script, ComplexityInfo(verifierComplexity, callableComplexities, maxComplexity)) =>
                  Json.obj(
                    "script"               -> script.bytes().base64,
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

  def compileWithImports: Route = path("script" / "compileWithImports") {
    import ScriptWithImportsRequest._
    (post & entity(as[ScriptWithImportsRequest])) { req =>
      def stdLib: StdLibVersion =
        if (blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls, blockchain.height)) {
          V5
        } else if (blockchain.isFeatureActivated(BlockchainFeatures.Ride4DApps, blockchain.height)) {
          V4
        } else {
          StdLibVersion.VersionDic.default
        }
      executeLimited(ScriptCompiler.compile(req.script, estimator(), req.imports, stdLib)) { result =>
        complete(
          checkInvokeExpression(result)
            .fold(
              e => ScriptCompilerError(e), {
                case (script, complexity) =>
                  Json.obj(
                    "script"     -> script.bytes().base64,
                    "complexity" -> complexity,
                    "extraFee"   -> extraFee(complexity)
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
            Script.complexityInfo(script, estimator(), useContractVerifierLimit = false).map((script, _))
          }
      ) { result =>
        complete(
          checkInvokeExpression(result)
            .fold(
              e => ScriptCompilerError(e), {
                case (script, ComplexityInfo(verifierComplexity, callableComplexities, maxComplexity)) =>
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
    (path("script" / "evaluate" / ScriptedAddress) & jsonPostD[JsObject]) { (address, obj) =>
      val script = blockchain.accountScript(address).get.script

      def parseCall(js: JsReadable) = {
        val binaryCall = js
          .asOpt[ByteStr]
          .toRight(GenericError("Unable to parse expr bytes"))
          .flatMap(ScriptCallEvaluator.parseBinaryCall)

        val textCall = js
          .asOpt[String]
          .toRight(GenericError("Unable to read expr string"))
          .flatMap(ScriptCallEvaluator.compile(script.stdLibVersion))

        binaryCall.orElse(textCall)
      }

      val result =
        for {
          expr                 <- parseCall(obj \ "expr")
          (result, complexity) <- ScriptCallEvaluator.executeExpression(blockchain, script, address, settings.evaluateScriptComplexityLimit)(expr)
        } yield Json.obj("result" -> ScriptValuesJson.serializeValue(result), "complexity" -> complexity)

      val requestData = obj ++ Json.obj("address" -> address.toString)
      val responseJson = result
        .recover {
          case e: ScriptExecutionError => Json.obj("error" -> ApiError.ScriptExecutionError.Id, "message" -> e.error)
          case other                   => ApiError.fromValidationError(other).json
        }
        .explicitGet() ++ requestData

      complete(responseJson)
    }

  private[this] val ScriptedAddress: PathMatcher1[Address] = AddrSegment.map {
    case address: Address if blockchain.hasAccountScript(address) => address
    case other =>
      throw ApiException(CustomValidationError(s"Address $other is not dApp"))
  }
}

object UtilsApiRoute {
  val MaxSeedSize     = 1024
  val DefaultSeedSize = 32

  private object ScriptCallEvaluator {
    def compile(stdLibVersion: StdLibVersion)(str: String): Either[GenericError, EXPR] = {
      val ctx =
        PureContext.build(stdLibVersion, fixUnicodeFunctions = true).withEnvironment[Environment] |+|
          CryptoContext.build(Global, stdLibVersion).withEnvironment[Environment] |+|
          WavesContext.build(Global, DirectiveSet(stdLibVersion, Account, Expression).explicitGet())

      ExpressionCompiler
        .compileUntyped(str, ctx.compilerContext.copy(arbitraryDeclarations = true))
        .leftMap(GenericError(_))
    }

    def parseBinaryCall(bs: ByteStr): Either[ValidationError, EXPR] = {
      Serde
        .deserialize(bs.arr)
        .left
        .map(GenericError(_))
        .map(_._1)
    }

    def executeExpression(blockchain: Blockchain, script: Script, address: Address, limit: Int)(
        expr: EXPR
    ): Either[ValidationError, (EVALUATED, Int)] = {
      for {
        ds <- DirectiveSet(script.stdLibVersion, Account, DAppType).leftMap(GenericError(_))
        ctx = BlockchainContext
          .build(
            ds,
            new DAppEnvironment(
              AddressScheme.current.chainId,
              Coeval.raiseError(new IllegalStateException("No input entity available")),
              Coeval.evalOnce(blockchain.height),
              blockchain,
              Coproduct[Environment.Tthis](Recipient.Address(ByteStr(address.bytes))),
              ds,
              new InvokeScriptTransactionLike {
                override def dApp: AddressOrAlias = address

                override def funcCall: Terms.FUNCTION_CALL = Terms.FUNCTION_CALL(FunctionHeader.User(""), Nil)

                override def payments: Seq[InvokeScriptTransaction.Payment] = Seq.empty

                override def root: InvokeScriptTransactionLike = this

                override def sender: PublicKey = PublicKey(ByteStr(new Array[Byte](32)))

                override def assetFee: (Asset, Long) = Asset.Waves -> 0L

                override def timestamp: Long = System.currentTimeMillis()

                override def chainId: Byte = AddressScheme.current.chainId

                override def id: Coeval[ByteStr] = Coeval.evalOnce(ByteStr.empty)

                override def checkedAssets: Seq[Asset.IssuedAsset] = Seq.empty
              },
              address,
              PublicKey(ByteStr.fill(KeyLength)(1)),
              Set.empty[Address],
              limitedExecution = false,
              limit,
              remainingCalls = ContractLimits.MaxSyncDAppCalls(script.stdLibVersion),
              availableActions = ContractLimits.MaxCallableActionsAmount(script.stdLibVersion),
              availableData = ContractLimits.MaxWriteSetSize(script.stdLibVersion),
              availableDataSize = ContractLimits.MaxTotalWriteSetSizeInBytes,
              currentDiff = Diff.empty,
              invocationRoot = DAppEnvironment.InvocationTreeTracker(DAppEnvironment.DAppInvocation(address, null, Nil))
            )
          )
        call = ContractEvaluator.buildSyntheticCall(script.expr.asInstanceOf[DApp], expr)
        limitedResult <- EvaluatorV2
          .applyLimitedCoeval(call, limit, ctx, script.stdLibVersion, checkConstructorArgsTypes = true)
          .value()
          .leftMap { case (err, _, log) => ScriptExecutionError.dAppExecution(err, log) }
        result <- limitedResult match {
          case (eval: EVALUATED, unusedComplexity, _) => Right((eval, limit - unusedComplexity))
          case (_: EXPR, _, log)                      => Left(ScriptExecutionError.dAppExecution(s"Calculation complexity limit exceeded", log))
        }
      } yield result
    }
  }
}
