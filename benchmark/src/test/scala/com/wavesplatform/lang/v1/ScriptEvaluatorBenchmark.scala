package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import cats.data.EitherT
import cats.kernel.Monoid
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.{BYTEVECTOR, LIST}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx.{CaseObj, CaseType, EvaluationContext, LazyVal}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
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

object ScriptEvaluatorBenchmark {
  val BodySize = 1024

  val TxType = CaseType(
    "Transaction",
    List(
      "bodyBytes"       -> BYTEVECTOR,
      "senderPublicKey" -> BYTEVECTOR,
      "proofs"          -> LIST(BYTEVECTOR)
    )
  )

  def contextStub: EvaluationContext = {
    val (sk, pk)  = Curve25519.createKeyPair("seed".getBytes())
    val bodyBytes = new Array[Byte](BodySize)
    Random.nextBytes(bodyBytes)
    val signature = ByteVector(Curve25519.sign(sk, bodyBytes))
    val proofs = (1 to 6).map { _ =>
      val proof = new Array[Byte](64)
      Random.nextBytes(proof)
      ByteVector(proof)
    }

    val txObj = CaseObj(
      TxType.typeRef,
      Map(
        "bodyBytes" -> ByteVector(bodyBytes),
        "senderPk"  -> ByteVector(pk),
        "proofs"    -> (signature +: proofs :+ ByteVector.empty) // real signature, 6 random byte strings, empty proof
      )
    )

    Monoid.combineAll(
      Seq(
        PureContext.evalContext,
        CryptoContext.build(Global).evaluationContext,
        EvaluationContext.build(typeDefs = Map.empty, letDefs = Map("tx" -> LazyVal(EitherT.pure(txObj))), functions = Seq.empty)
      ))
  }
}

@State(Scope.Benchmark)
class NestedBlocks {
  val context: EvaluationContext = PureContext.evalContext

  val expr: EXPR = {
    val blockCount = 284 // yields script of complexity 1998
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
    val base58Count = 75 // yields script of 8171 bytes
    val lets = (1 to base58Count)
      .map { i =>
        val b = new Array[Byte](64)
        Random.nextBytes(b)
        s"let v$i = size(toBase58String(base58'${Base58.encode(b)}'))\n"
      }
      .reduce(_ + _)
    val sum    = (1 to base58Count).map(i => s"v$i").reduce(_ + " + " + _)
    val script = lets + sum + " == 0"
    ScriptCompiler(script).explicitGet()._1.expr.asInstanceOf[EXPR]
  }

  val decode: EXPR = {
    val base58Length = 5950 // yields script of 8169 bytes
    val b            = new Array[Byte](base58Length)
    Random.nextBytes(b)
    val script = s"""size(fromBase58String(\"${Base58.encode(b)}\")) == 0"""
    ScriptCompiler(script).explicitGet()._1.expr.asInstanceOf[EXPR]
  }
}

@State(Scope.Benchmark)
class Signatures {
  import ScriptEvaluatorBenchmark._
  val context: EvaluationContext = contextStub

  val expr: EXPR = {
    val sigCount = 17 // yields script of complexity 1920
    val script = (1 to sigCount)
      .map { i =>
        val pk = new Array[Byte](32)
        Random.nextBytes(pk)
        (pk, i)
      }
      .map {
        case (pk, i) =>
          s"sigVerify(tx.bodyBytes, tx.proofs[${i % 7}], base58'${Base58.encode(pk)}')"
      }
      .reduce(_ + " ||\n" + _)
    ScriptCompiler(script).explicitGet()._1.expr.asInstanceOf[EXPR]
  }
}
