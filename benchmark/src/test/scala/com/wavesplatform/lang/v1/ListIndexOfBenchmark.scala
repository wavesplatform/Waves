package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.EnvironmentFunctionsBenchmark.{curve25519, randomBytes}
import com.wavesplatform.lang.v1.ListIndexOfBenchmark.{CurveSt32k, ListIndexOfSt}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_STRING, EVALUATED}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scorex.crypto.signatures.{Curve25519, Signature}

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 30)
@Measurement(iterations = 30)
class ListIndexOfBenchmark {
  @Benchmark
  def indexOfMaxSizeElementInMaxCmpWeightElementsList(st: ListIndexOfSt, bh: Blackhole): Unit =
    bh.consume(st.indexOf(st.listWithMaxCmpWeightElements, st.maxSizeElementToFound))

  @Benchmark
  def indexOfMaxCmpWeightElementInMaxSizeElementsList(st: ListIndexOfSt, bh: Blackhole): Unit =
    bh.consume(st.indexOf(st.listWithMaxSizeElements, st.maxCmpWeightElementToFound))

  @Benchmark
  def lastIndexOfMaxSizeElementInMaxCmpWeightElementsList(st: ListIndexOfSt, bh: Blackhole): Unit =
    bh.consume(st.lastIndexOf(st.listWithMaxCmpWeightElements, st.maxSizeElementToFound))

  @Benchmark
  def lastIndexOfMaxCmpWeightElementInMaxSizeElementsList(st: ListIndexOfSt, bh: Blackhole): Unit =
    bh.consume(st.lastIndexOf(st.listWithMaxSizeElements, st.maxCmpWeightElementToFound))

  @Benchmark
  def sigVerify32Kb(st: CurveSt32k, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(Signature @@ st.signature, st.message, st.publicKey))
}

object ListIndexOfBenchmark {
  @State(Scope.Benchmark)
  class ListIndexOfSt {
    val maxCmpWeightElementToFound   = CONST_STRING("a" * ContractLimits.MaxCmpWeight.toInt).explicitGet()
    val maxSizeElementToFound        = CONST_STRING("a" * 150 * 1024).explicitGet()
    val listWithMaxCmpWeightElements = IndexedSeq.fill(1000)(CONST_STRING("a" * (ContractLimits.MaxCmpWeight.toInt - 1) + "b").explicitGet())
    val listWithMaxSizeElements      = IndexedSeq.fill(1000)(CONST_STRING(("a" * (150 * 1024 - 1)) + "b").explicitGet())

    def indexOf(list: Seq[EVALUATED], element: EVALUATED): Either[String, EVALUATED] =
      PureContext.genericListIndexOf(element, list.indexOf, list.indexWhere)

    def lastIndexOf(list: Seq[EVALUATED], element: EVALUATED): Either[String, EVALUATED] =
      PureContext.genericListIndexOf(element, list.lastIndexOf(_), list.lastIndexWhere)
  }

  @State(Scope.Benchmark)
  class CurveSt32k {
    val (privateKey, publicKey) = curve25519.generateKeypair
    val message                 = randomBytes(32 * 1024)
    val signature               = curve25519.sign(privateKey, message)
  }
}
