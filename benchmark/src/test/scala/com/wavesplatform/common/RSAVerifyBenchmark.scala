package com.wavesplatform.common

import java.security.{KeyPairGenerator, SecureRandom, Signature => JavaSignature}
import java.util.concurrent.TimeUnit

import com.wavesplatform.common.RSAVerifyBenchmark.{CurveSt, RSASt}
import com.wavesplatform.lang.v1.EnvironmentFunctionsBenchmark.{curve25519, randomBytes}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.SHA3512
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scorex.crypto.signatures.{Curve25519, Signature}

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class RSAVerifyBenchmark {

  @Benchmark
  def sigVerify150Kb(st: CurveSt, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(Signature @@ st.signature150Kb, st.message150Kb, st.publicKey))

  @Benchmark
  def sigVerify32Kb(st: CurveSt, bh: Blackhole): Unit =
    bh.consume(Curve25519.verify(Signature @@ st.signature32Kb, st.message32Kb, st.publicKey))

  @Benchmark
  def rsaVerify_SHA3512_16384_32Kb(st: RSASt, bh: Blackhole): Unit =
    bh.consume(RSA.verify(st.alg, st.message32Kb, st.signature, st.publicKey))
}

object RSAVerifyBenchmark {
  @State(Scope.Benchmark)
  class CurveSt {
    val (privateKey, publicKey) = curve25519.generateKeypair
    val message150Kb            = randomBytes(150 * 1024)
    val message32Kb             = randomBytes(32 * 1024)
    val signature150Kb          = curve25519.sign(privateKey, message150Kb)
    val signature32Kb           = curve25519.sign(privateKey, message32Kb)
  }

  @State(Scope.Benchmark)
  class RSASt {
    val alg = SHA3512
    val message32Kb = randomBytes(32 * 1024)

    val pair = {
      val generator = KeyPairGenerator.getInstance("RSA")
      generator.initialize(16 * 1024, new SecureRandom)
      generator.generateKeyPair()
    }
    val publicKey = pair.getPublic.getEncoded

    val signature = {
      val privateKey = pair.getPrivate
      val prefix = RSA.digestAlgorithmPrefix(alg)
      val provider = new BouncyCastleProvider()

      val privateSignature = JavaSignature.getInstance(s"${prefix}withRSA", provider)
      privateSignature.initSign(privateKey)
      privateSignature.update(message32Kb)
      privateSignature.sign
    }
  }
}
