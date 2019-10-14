package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import cats.Id
import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.values.V1
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.ScriptEvaluatorBenchmark._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1._
import com.wavesplatform.lang.v1.evaluator.FunctionIds.{FROMBASE58, SIGVERIFY, TOBASE58}
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scorex.crypto.signatures.Curve25519

import scala.util.Random

object ScriptEvaluatorBenchmark {
  val version = V1
  val pureEvalContext: EvaluationContext[NoContext, Id] = PureContext.build(Global, V1).evaluationContext
  val evaluatorV1: EvaluatorV1[Id, NoContext] = new EvaluatorV1[Id, NoContext]()
}

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 20)
@Measurement(iterations = 10)
class ScriptEvaluatorBenchmark {
  @Benchmark
  def bigSum(st: BigSum, bh: Blackhole): Unit = bh.consume(evaluatorV1.apply[EVALUATED](pureEvalContext, st.expr))

  @Benchmark
  def nestedBlocks(st: NestedBlocks, bh: Blackhole): Unit = bh.consume(evaluatorV1.apply[EVALUATED](st.context, st.expr))

  @Benchmark
  def signatures(st: Signatures, bh: Blackhole): Unit = bh.consume(evaluatorV1.apply[EVALUATED](st.context, st.expr))

  @Benchmark
  def base58encode(st: Base58Perf, bh: Blackhole): Unit = bh.consume(evaluatorV1.apply[EVALUATED](st.context, st.encode))

  @Benchmark
  def base58decode(st: Base58Perf, bh: Blackhole): Unit = bh.consume(evaluatorV1.apply[EVALUATED](st.context, st.decode))

  @Benchmark
  def stringConcat(st: Concat, bh: Blackhole): Unit = bh.consume(evaluatorV1.apply[EVALUATED](st.context, st.strings))

  @Benchmark
  def bytesConcat(st: Concat, bh: Blackhole): Unit = bh.consume(evaluatorV1.apply[EVALUATED](st.context, st.bytes))
}

@State(Scope.Benchmark)
class NestedBlocks {
  val context: EvaluationContext[NoContext, Id] = pureEvalContext

  val expr: EXPR = {
    val blockCount = 300
    val cond       = FUNCTION_CALL(PureContext.eq, List(REF(s"v$blockCount"), CONST_LONG(0)))
    val blocks = (1 to blockCount).foldRight[EXPR](cond) { (i, e) =>
      BLOCK(LET(s"v$i", REF(s"v${i - 1}")), e)
    }
    BLOCK(LET("v0", CONST_LONG(0)), blocks)
  }
}

@State(Scope.Benchmark)
class Base58Perf {
  val context: EvaluationContext[NoContext, Id] =
    Monoid.combine(pureEvalContext, CryptoContext.build(Global, version).evaluationContext)

  val encode: EXPR = {
    val base58Count = 120
    val sum = (1 to base58Count).foldRight[EXPR](CONST_LONG(0)) {
      case (i, e) => FUNCTION_CALL(PureContext.sumLong, List(REF("v" + i), e))
    }
    (1 to base58Count)
      .map { i =>
        val b = new Array[Byte](64)
        Random.nextBytes(b)
        LET(
          "v" + i,
          FUNCTION_CALL(
            PureContext.sizeString,
            List(FUNCTION_CALL(Native(TOBASE58), List(CONST_BYTESTR(ByteStr(b)).explicitGet())))
          )
        )
      }
      .foldRight[EXPR](sum) { case (let, e) => BLOCK(let, e) }
  }

  val decode: EXPR = {
    val base58Count = 60
    val sum = (1 to base58Count).foldRight[EXPR](CONST_LONG(0)) {
      case (i, e) => FUNCTION_CALL(PureContext.sumLong, List(REF("v" + i), e))
    }
    (1 to base58Count)
      .map { i =>
        val b = new Array[Byte](64)
        Random.nextBytes(b)
        LET("v" + i, FUNCTION_CALL(PureContext.sizeBytes, List(FUNCTION_CALL(Native(FROMBASE58), List(CONST_STRING(Base58.encode(b)).explicitGet())))))
      }
      .foldRight[EXPR](sum) { case (let, e) => BLOCK(let, e) }
  }
}

@State(Scope.Benchmark)
class Signatures {
  val context: EvaluationContext[NoContext, Id] =
    Monoid.combine(pureEvalContext, CryptoContext.build(Global, version).evaluationContext)

  val expr: EXPR = {
    val sigCount = 20
    val sum = (1 to sigCount).foldRight[EXPR](CONST_LONG(0)) {
      case (i, e) => FUNCTION_CALL(PureContext.sumLong, List(REF("v" + i), e))
    }
    (1 to sigCount)
      .map { i =>
        val msg = new Array[Byte](1024)
        Random.nextBytes(msg)
        val seed = new Array[Byte](256)
        Random.nextBytes(seed)
        val (sk, pk) = Curve25519.createKeyPair(seed)
        val sig      = Curve25519.sign(sk, msg)

        LET(
          "v" + i,
          IF(
            FUNCTION_CALL(
              Native(SIGVERIFY),
              List(
                CONST_BYTESTR(ByteStr(msg)).explicitGet(),
                CONST_BYTESTR(ByteStr(sig)).explicitGet(),
                CONST_BYTESTR(ByteStr(pk)).explicitGet()
              )
            ),
            CONST_LONG(1),
            CONST_LONG(0)
          )
        )
      }
      .foldRight[EXPR](FUNCTION_CALL(PureContext.eq, List(sum, CONST_LONG(sigCount)))) {
        case (let, e) => BLOCK(let, e)
      }
  }
}

@State(Scope.Benchmark)
class Concat {
  val context: EvaluationContext[NoContext, Id] = pureEvalContext

  private val Steps = 180

  private def expr(init: EXPR, func: FunctionHeader, operand: EXPR, count: Int) =
    (1 to count).foldLeft[EXPR](init) {
      case (e, _) => FUNCTION_CALL(func, List(e, operand))
    }

  val strings: EXPR = expr(
    CONST_STRING("a" * (Short.MaxValue - Steps)).explicitGet(),
    PureContext.sumString,
    CONST_STRING("a").explicitGet(),
    Steps
  )

  val bytes: EXPR =
    expr(
      CONST_BYTESTR(ByteStr.fill(Short.MaxValue - Steps)(0)).explicitGet(),
      PureContext.sumByteStr,
      CONST_BYTESTR(ByteStr.fromBytes(0)).explicitGet(),
      Steps
    )
}
