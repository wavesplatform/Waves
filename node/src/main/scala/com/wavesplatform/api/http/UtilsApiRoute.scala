package com.wavesplatform.api.http

import java.security.SecureRandom

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.api.http.ApiError.{ScriptCompilerError, TooBigArrayAllocation}
import com.wavesplatform.common.utils._
import com.wavesplatform.crypto
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.utils.Time
import monix.execution.Scheduler
import play.api.libs.json._

case class UtilsApiRoute(
    timeService: Time,
    settings: RestAPISettings,
    estimator: ScriptEstimator,
    limitedScheduler: Scheduler
) extends ApiRoute
    with AuthRoute
    with TimeLimitedRoute {

  import UtilsApiRoute._

  private def seed(length: Int) = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Json.obj("seed" -> Base58.encode(seed))
  }

  override val route: Route = pathPrefix("utils") {
    decompile ~ compile ~ compileCode ~ compileWithImports ~ scriptMeta ~ estimate ~ time ~ seedRoute ~ length ~ hashFast ~ hashSecure ~ sign ~ transactionSerialize
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
      parameter('assetScript.as[Boolean] ? false) { isAssetScript =>
        executeLimited(ScriptCompiler(code, isAssetScript, estimator)) { result =>
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
      executeLimited(ScriptCompiler.compile(code, estimator)) { result =>
        complete(
          result
            .fold(
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

  def compileWithImports: Route = path("script" / "compileWithImports") {
    import ScriptWithImportsRequest._
    (post & entity(as[ScriptWithImportsRequest])) { req =>
      executeLimited(ScriptCompiler.compile(req.script, estimator, req.imports)) { result =>
        complete(
          result
            .fold(
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

  def estimate: Route = path("script" / "estimate") {
    (post & entity(as[String])) { code =>
      executeLimited(
        Script
          .fromBase64String(code)
          .left
          .map(_.m)
          .flatMap { script =>
            Script.estimate(script, estimator).map((script, _))
          }
      ) { result =>
        complete(
          result.fold(
            e => ScriptCompilerError(e), {
              case (script, complexity) =>
                Json.obj(
                  "script"     -> code,
                  "scriptText" -> script.expr.toString, // [WAIT] Script.decompile(script),
                  "complexity" -> complexity,
                  "extraFee"   -> FeeValidation.ScriptExtraFee
                )
            }
          )
        )
      }
    }
  }

  def scriptMeta: Route = path("script" / "meta") {
    (post & entity(as[String])) { code =>
      val result: ToResponseMarshallable = Global
        .scriptMeta(code) // Does not estimate complexity, therefore it should not hang
        .map(metaConverter.foldRoot)
        .fold(e => e, r => r)
      complete(result)
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

  def sign: Route = (path("sign" / Segment) & post) { pk =>
    entity(as[String]) { message =>
      complete(
        Json.obj(
          "message" -> message,
          "signature" ->
            Base58.encode(crypto.sign(PrivateKey(Base58.tryDecodeWithLimit(pk).get), Base58.tryDecodeWithLimit(message).get))
        )
      )
    }
  }

  def transactionSerialize: Route =
    path("transactionSerialize")(jsonPost[JsObject] { jsv =>
      parseOrCreateTransaction(jsv)(tx => Json.obj("bytes" -> tx.bodyBytes().map(_.toInt & 0xff)))
    })
}

object UtilsApiRoute {
  val MaxSeedSize     = 1024
  val DefaultSeedSize = 32
}
