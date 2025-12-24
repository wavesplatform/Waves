package com.wavesplatform.common

import com.wavesplatform.common.utils.EitherExt2.explicitGet
import com.wavesplatform.crypto.{P256Curve, Sha256}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(value = 1/*, jvmArgsAppend = Array("-Djava.security.debug=all")*/)
@Warmup(iterations = 30, time = 1)
@Measurement(iterations = 30, time = 1)
class P256VerifyBenchmark {
  import P256VerifyBenchmark.*
  @Benchmark
  def p256VerifyRaw(bh: Blackhole): Unit = bh.consume(require(P256Curve.verify(hash, signature, publicKey) == Right(true)))

  @Benchmark
  def validateCertChain(bh: Blackhole): Unit = bh.consume(P256Curve.validateCertChain(certChain, crls, ts))
}

object P256VerifyBenchmark {
  private def res(name: String) = getClass.getResourceAsStream(name).readAllBytes()

  val message   = res("qe-report.bin")
  val signature = res("qe-report.sig.bin")
  val certChain = Seq(res("sgx-cert.bin"), res("platform-cert.bin"), res("root-cert.bin"))
  val crls      = Seq(res("IntelSGXRootCA.der"), res("pckcrl"))
  val ts        = 1769499118300L
  val publicKey = P256Curve.validateCertChain(certChain, crls, ts).explicitGet()
  val hash = Sha256.hash(message)

}
