package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import cats.kernel.Monoid
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.FunctionIds.{FROMBASE58, TOBASE58, SIGVERIFY}
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.utils.Base58
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scodec.bits.ByteVector
import scorex.crypto.signatures.Curve25519

import scala.util.Random

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 20)
@Measurement(iterations = 10)
class ScriptEvaluatorBenchmark {
  @Benchmark
  def bigSum(st: BigSum, bh: Blackhole): Unit = bh.consume(EvaluatorV1[Boolean](PureContext.evalContext, st.expr))

  @Benchmark
  def nestedBlocks(st: NestedBlocks, bh: Blackhole): Unit = bh.consume(EvaluatorV1[Boolean](st.context, st.expr))

  @Benchmark
  def signatures(st: Signatures, bh: Blackhole): Unit = bh.consume(EvaluatorV1[Boolean](st.context, st.expr))

  @Benchmark
  def base58encode(st: Base58Perf, bh: Blackhole): Unit = bh.consume(EvaluatorV1[Boolean](st.context, st.encode))

  @Benchmark
  def base58decode(st: Base58Perf, bh: Blackhole): Unit = bh.consume(EvaluatorV1[Boolean](st.context, st.decode))
}

@State(Scope.Benchmark)
class NestedBlocks {
  val context: EvaluationContext = PureContext.evalContext

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
  val context: EvaluationContext = Monoid.combine(PureContext.evalContext, CryptoContext.build(Global).evaluationContext)

  val encode: EXPR = {
    val base58Count = 120
    val sum = (1 to base58Count).foldRight[EXPR](CONST_LONG(0)) {
      case (i, e) => FUNCTION_CALL(PureContext.sumLong, List(REF("v" + i), e))
    }
    (1 to base58Count)
      .map { i =>
        val b = new Array[Byte](64)
        Random.nextBytes(b)
        LET("v" + i, FUNCTION_CALL(PureContext.sizeString, List(FUNCTION_CALL(Native(TOBASE58), List(CONST_BYTEVECTOR(ByteVector(b)))))))
      }
      .foldRight[EXPR](FUNCTION_CALL(PureContext.eq, List(sum, CONST_LONG(base58Count)))) {
        case (let, e) => BLOCK(let, e)
      }
  }

  val decode: EXPR = {
    val base58Length = 6000
    val b            = new Array[Byte](base58Length)
    Random.nextBytes(b)
    FUNCTION_CALL(
      PureContext.eq,
      List(FUNCTION_CALL(PureContext.sizeBytes, List(FUNCTION_CALL(Native(FROMBASE58), List(CONST_STRING(Base58.encode(b)))))),
           CONST_LONG(base58Length))
    )
  }
}

@State(Scope.Benchmark)
class Signatures {
  val context: EvaluationContext = Monoid.combine(PureContext.evalContext, CryptoContext.build(Global).evaluationContext)

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
            FUNCTION_CALL(Native(SIGVERIFY),
                          List(CONST_BYTEVECTOR(ByteVector(msg)), CONST_BYTEVECTOR(ByteVector(sig)), CONST_BYTEVECTOR(ByteVector(pk)))),
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
