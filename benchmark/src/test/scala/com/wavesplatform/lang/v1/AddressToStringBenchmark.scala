package com.wavesplatform.lang.v1

import java.util.concurrent.TimeUnit

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.crypto.Curve25519
import com.wavesplatform.lang.v1.AddressToStringBenchmark.AddressToString
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.PureFunctionsRebenchmark.evalV5
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, CaseObj, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class AddressToStringBenchmark {
  @Benchmark
  def addressToString(bh: Blackhole, st: AddressToString): Unit =
    bh.consume(evalV5(st.expr))
}

object AddressToStringBenchmark {
  @State(Scope.Benchmark)
  class AddressToString {
    val publicKey = new Array[Byte](Curve25519.KeyLength)
    Random.nextBytes(publicKey)

    val address = Address.fromPublicKey(PublicKey(publicKey)).bytes

    val expr =
      FUNCTION_CALL(
        Native(FunctionIds.ADDRESSTOSTRING),
        List(CaseObj(Types.addressType, Map("bytes" -> CONST_BYTESTR(ByteStr(address)).explicitGet())))
      )
  }
}
