package com.wavesplatform

import cats.kernel.Monoid
import com.google.common.base.Throwables
import com.wavesplatform.db.{Storage, VersionedStorage}
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.{FunctionHeader, ScriptEstimator}
import com.wavesplatform.lang.v1.compiler.CompilerContext
import com.wavesplatform.lang.v1.compiler.CompilerContext._
import com.wavesplatform.lang.v1.compiler.Terms.TRUE
import com.wavesplatform.lang.v1.evaluator.ctx.{CaseType, EvaluationContext, NativeFunction, UserFunction}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import monix.eval.Coeval
import monix.execution.UncaughtExceptionReporter
import org.joda.time.Duration
import org.joda.time.format.PeriodFormat
import scorex.account.AddressScheme
import scorex.transaction.smart.{BlockchainContext, WavesEnvironment}
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.util.Try

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

  lazy val dummyNetworkByte: Byte                    = AddressScheme.current.chainId
  lazy val dummyEvaluationContext: EvaluationContext = BlockchainContext.build(dummyNetworkByte, Coeval(???), Coeval(???), null)
  lazy val functionCosts: Map[FunctionHeader, Coeval[Long]] = {
    var costs: Map[FunctionHeader, Coeval[Long]] = dummyEvaluationContext.typeDefs.collect {
      case (typeName, CaseType(_, fields)) => FunctionHeader.User(typeName) -> Coeval.now(fields.size.toLong)
    }
    dummyEvaluationContext.functions.values.foreach { func =>
      val cost = func match {
        case f: UserFunction =>
          import f.signature.args
          Coeval.evalOnce(ScriptEstimator(costs, f.ev(args.map(_ => TRUE).toList)).right.get - args.size)
        case f: NativeFunction => Coeval.now(f.cost)
      }
      costs += func.header -> cost
    }
    costs
  }

  lazy val dummyCompilerContext: CompilerContext =
    Monoid.combineAll(
      Seq(
        CryptoContext.compilerContext(Global),
        WavesContext.build(new WavesEnvironment(dummyNetworkByte, Coeval(???), Coeval(???), null)).compilerContext,
        PureContext.compilerContext
      ))

}
