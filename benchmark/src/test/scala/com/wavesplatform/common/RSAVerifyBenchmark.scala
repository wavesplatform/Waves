package com.wavesplatform.common

import com.wavesplatform.common.RSAVerifyBenchmark.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.*
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import com.wavesplatform.utils.randomBytes

import java.security.{KeyPairGenerator, Signature as JavaSignature}
import java.util.concurrent.TimeUnit

trait AlgSHA3512 {
  def alg: DigestAlgorithm = SHA3512
}
trait AlgSHA3256 {
  def alg: DigestAlgorithm = SHA3256
}

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(value = 1, jvmArgsAppend = Array("-XX:+UnlockDiagnosticVMOptions", "-XX:-UseSHA3Intrinsics"))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class RSAVerifyBenchmark {
  @Benchmark
  def rsaVerify_SHA3512_16384_32Kb(st: RSASt_SHA3512_32Kb, bh: Blackhole): Unit =
    bh.consume(RSA.verify(st.alg, st.message, st.signature, st.publicKey))
  @Benchmark
  def rsaVerify_SHA3512_16384_64Kb(st: RSASt_SHA3512_64Kb, bh: Blackhole): Unit =
    bh.consume(RSA.verify(st.alg, st.message, st.signature, st.publicKey))
  @Benchmark
  def rsaVerify_SHA3512_16384_96Kb(st: RSASt_SHA3512_96Kb, bh: Blackhole): Unit =
    bh.consume(RSA.verify(st.alg, st.message, st.signature, st.publicKey))
  @Benchmark
  def rsaVerify_SHA3512_16384_128Kb(st: RSASt_SHA3512_128Kb, bh: Blackhole): Unit =
    bh.consume(RSA.verify(st.alg, st.message, st.signature, st.publicKey))
  @Benchmark
  def rsaVerify_SHA3256_16384_32Kb(st: RSASt_SHA3256_32Kb, bh: Blackhole): Unit =
    bh.consume(RSA.verify(st.alg, st.message, st.signature, st.publicKey))
  @Benchmark
  def rsaVerify_SHA3256_16384_48Kb(st: RSASt_SHA3256_48Kb, bh: Blackhole): Unit =
    bh.consume(RSA.verify(st.alg, st.message, st.signature, st.publicKey))
  @Benchmark
  def rsaVerify_SHA3256_16384_64Kb(st: RSASt_SHA3256_64Kb, bh: Blackhole): Unit =
    bh.consume(RSA.verify(st.alg, st.message, st.signature, st.publicKey))
  @Benchmark
  def rsaVerify_SHA3256_16384_96Kb(st: RSASt_SHA3256_96Kb, bh: Blackhole): Unit =
    bh.consume(RSA.verify(st.alg, st.message, st.signature, st.publicKey))
  @Benchmark
  def rsaVerify_SHA3256_16384_128Kb(st: RSASt_SHA3256_128Kb, bh: Blackhole): Unit =
    bh.consume(RSA.verify(st.alg, st.message, st.signature, st.publicKey))
}

object RSAVerifyBenchmark {
  @State(Scope.Benchmark)
  class RSASt_SHA3512_32Kb extends RSASt(SHA3512, 32)

  @State(Scope.Benchmark)
  class RSASt_SHA3512_64Kb extends RSASt(SHA3512, 64)

  @State(Scope.Benchmark)
  class RSASt_SHA3512_96Kb extends RSASt(SHA3512, 96)

  @State(Scope.Benchmark)
  class RSASt_SHA3512_128Kb extends RSASt(SHA3512, 128)

  @State(Scope.Benchmark)
  class RSASt_SHA3256_32Kb extends RSASt(SHA3256, 32)

  @State(Scope.Benchmark)
  class RSASt_SHA3256_48Kb extends RSASt(SHA3256, 48)

  @State(Scope.Benchmark)
  class RSASt_SHA3256_64Kb extends RSASt(SHA3256, 64)

  @State(Scope.Benchmark)
  class RSASt_SHA3256_96Kb extends RSASt(SHA3256, 96)

  @State(Scope.Benchmark)
  class RSASt_SHA3256_128Kb extends RSASt(SHA3256, 128)

  val pair = KeyPairGenerator.getInstance("RSA").generateKeyPair()

  class RSASt(val alg: DigestAlgorithm, val messageSize: Int) {
    val message = randomBytes(messageSize * 1024)

    val publicKey = pair.getPublic.getEncoded

    val signature = {
      val privateKey = pair.getPrivate
      val prefix     = RSA.digestAlgorithmPrefix(alg)

      val privateSignature = JavaSignature.getInstance(s"${prefix}withRSA", "SunRsaSign")
      privateSignature.initSign(privateKey)
      privateSignature.update(message)
      privateSignature.sign
    }
  }
}
