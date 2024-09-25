package com.wavesplatform.common

import java.security.{KeyPairGenerator, SecureRandom, Signature => JavaSignature}
import java.util.concurrent.TimeUnit

import com.wavesplatform.common.RSAVerifyBenchmark._
import com.wavesplatform.lang.v1.EnvironmentFunctionsBenchmark.randomBytes
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA._
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

trait AlgSHA3512 {
  def alg: DigestAlgorithm = SHA3512
}
trait AlgSHA3256 {
  def alg: DigestAlgorithm = SHA3256
}

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 30)
@Measurement(iterations = 30)
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

  val pair = {
    val generator = KeyPairGenerator.getInstance("RSA")
    generator.initialize(16 * 1024, new SecureRandom)
    generator.generateKeyPair()
  }

  class RSASt(val alg: DigestAlgorithm, val messageSize: Int) {
    val message = randomBytes(messageSize * 1024)

    val publicKey = pair.getPublic.getEncoded

    val signature = {
      val privateKey = pair.getPrivate
      val prefix     = RSA.digestAlgorithmPrefix(alg)
      val provider   = new BouncyCastleProvider()

      val privateSignature = JavaSignature.getInstance(s"${prefix}withRSA", provider)
      privateSignature.initSign(privateKey)
      privateSignature.update(message)
      privateSignature.sign
    }
  }
}
