package com.wavesplatform

import java.security.SecureRandom

import cats.kernel.Monoid
import com.google.common.base.Throwables
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.state.ByteStr._
import com.wavesplatform.db.{Storage, VersionedStorage}
import com.wavesplatform.lang.{ContentType, Global, ScriptType}
import com.wavesplatform.lang.StdLibVersion._
import com.wavesplatform.lang.utils.DirectiveSet
import com.wavesplatform.lang.v1.compiler.{CompilerContext, DecompilerContext}
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import com.wavesplatform.transaction.smart.WavesEnvironment
import monix.eval.Coeval
import monix.execution.UncaughtExceptionReporter
import org.joda.time.Duration
import org.joda.time.format.PeriodFormat
import play.api.libs.json._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration._
import scala.reflect.runtime.universe
import scala.util.{Failure, Success, Try}

package object utils extends ScorexLogging {

  private val BytesMaxValue  = 256
  private val Base58MaxValue = 58

  private val BytesLog = math.log(BytesMaxValue)
  private val BaseLog  = math.log(Base58MaxValue)

  val UncaughtExceptionsToLogReporter = UncaughtExceptionReporter(exc => log.error(Throwables.getStackTraceAsString(exc)))

  def base58Length(byteArrayLength: Int): Int = math.ceil(BytesLog / BaseLog * byteArrayLength).toInt

  def createWithVerification[A <: Storage with VersionedStorage](storage: => A): Try[A] = Try {
    if (storage.isVersionValid) storage
    else {
      log.info(s"Re-creating storage")
      val b = storage.createBatch()
      storage.removeEverything(b)
      storage.commit(b)
      storage
    }
  }

  def forceStopApplication(reason: ApplicationStopReason = Default): Unit =
    new Thread(() => {
      System.exit(reason.code)
    }, "waves-platform-shutdown-thread").start()

  def humanReadableSize(bytes: Long, si: Boolean = true): String = {
    val (baseValue, unitStrings) =
      if (si)
        (1000, Vector("B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"))
      else
        (1024, Vector("B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"))

    def getExponent(curBytes: Long, baseValue: Int, curExponent: Int = 0): Int =
      if (curBytes < baseValue) curExponent
      else {
        val newExponent = 1 + curExponent
        getExponent(curBytes / (baseValue * newExponent), baseValue, newExponent)
      }

    val exponent   = getExponent(bytes, baseValue)
    val divisor    = Math.pow(baseValue, exponent)
    val unitString = unitStrings(exponent)

    f"${bytes / divisor}%.1f $unitString"
  }

  def humanReadableDuration(duration: Long): String = {
    val d = new Duration(duration)
    PeriodFormat.getDefault.print(d.toPeriod)
  }

  implicit class Tap[A](a: A) {
    def tap(g: A => Unit): A = {
      g(a)
      a
    }
  }

  private val lazyAssetContexts: Map[StdLibVersion, Coeval[CTX]] =
    Seq
      .tabulate(2) { v =>
        val version = com.wavesplatform.lang.StdLibVersion.parseVersion(v + 1)
        version -> Coeval.evalOnce(
          Monoid
            .combineAll(Seq(
              PureContext.build(version),
              CryptoContext.build(Global),
              WavesContext
                .build(
                  DirectiveSet(version, ScriptType.Asset, ContentType.Expression),
                  new WavesEnvironment(AddressScheme.current.chainId, Coeval(???), Coeval(???), EmptyBlockchain, Coeval(???))
                )
            )))
      }
      .toMap

  private val lazyContexts: Map[StdLibVersion, Coeval[CTX]] =
    Seq
      .tabulate(3) { v =>
        val version: StdLibVersion = com.wavesplatform.lang.StdLibVersion(v + 1)
        version -> Coeval.evalOnce(
          Monoid
            .combineAll(Seq(
              PureContext.build(version),
              CryptoContext.build(Global),
              WavesContext.build(DirectiveSet(version, ScriptType.Account, ContentType.Contract), // TODO: why contract? should be DirectiveSet->Coeval[CTX]
                                 new WavesEnvironment(AddressScheme.current.chainId, Coeval(???), Coeval(???), EmptyBlockchain, Coeval(???)))
            )))
      }
      .toMap

  def dummyEvalContext(version: StdLibVersion): EvaluationContext = lazyContexts(version)().evaluationContext

  private val lazyFunctionCosts: Map[StdLibVersion, Coeval[Map[FunctionHeader, Coeval[Long]]]] =
    lazyContexts.map(el => (el._1, el._2.map(ctx => estimate(el._1, ctx.evaluationContext))))

  def functionCosts(version: StdLibVersion): Map[FunctionHeader, Coeval[Long]] = lazyFunctionCosts(version)()

  def estimate(version: StdLibVersion, ctx: EvaluationContext): Map[FunctionHeader, Coeval[Long]] = {
    val costs: mutable.Map[FunctionHeader, Coeval[Long]] = ctx.typeDefs.collect {
      case (typeName, CaseType(_, fields)) => FunctionHeader.User(typeName) -> Coeval.now(fields.size.toLong)
    }(collection.breakOut)

    ctx.functions.values.foreach { func =>
      val cost = func match {
        case f: UserFunction =>
          f.costByLibVersion(version)
        case f: NativeFunction => f.cost
      }
      costs += func.header -> Coeval.now(cost)
    }

    costs.toMap
  }

  def compilerContext(version: StdLibVersion, isAssetScript: Boolean): CompilerContext =
    if (isAssetScript) lazyAssetContexts(version)().compilerContext
    else lazyContexts(version)().compilerContext

  val defaultDecompilerContext: DecompilerContext = lazyContexts(V3)().decompilerContext

  def varNames(version: StdLibVersion): Set[String] = compilerContext(version, isAssetScript = false).varDefs.keySet

  @tailrec
  final def untilTimeout[T](timeout: FiniteDuration, delay: FiniteDuration = 100.milliseconds, onFailure: => Unit = {})(fn: => T): T = {
    Try {
      fn
    } match {
      case Success(x) => x
      case _ if timeout > delay =>
        Thread.sleep(delay.toMillis)
        untilTimeout(timeout - delay, delay, onFailure)(fn)
      case Failure(e) =>
        onFailure
        throw e
    }
  }

  def randomBytes(howMany: Int = 32): Array[Byte] = {
    val r = new Array[Byte](howMany)
    new SecureRandom().nextBytes(r) //overrides r
    r
  }

  def objectFromString[T](fullClassName: String): Try[T] = Try {
    val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)
    val module        = runtimeMirror.staticModule(fullClassName)
    val obj           = runtimeMirror.reflectModule(module)
    obj.instance.asInstanceOf[T]
  }

  @tailrec def doWhile[T](z: T)(cond: T => Boolean)(f: T => T): T = if (cond(z)) doWhile(f(z))(cond)(f) else z

  implicit val byteStrWrites: Format[ByteStr] = new Format[ByteStr] {

    override def writes(o: ByteStr): JsValue = JsString(o.base58)

    override def reads(json: JsValue): JsResult[ByteStr] = json match {
      case JsString(v) => decodeBase58(v).fold(e => JsError(s"Error parsing base58: ${e.getMessage}"), b => JsSuccess(b))
      case _           => JsError("Expected JsString")
    }
  }
}
