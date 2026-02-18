package com.wavesplatform.common

import com.google.common.primitives.Ints
import com.wavesplatform.common.merkle.Merkle.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.explicitGet
import com.wavesplatform.crypto.fastHash
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CONST_BYTESTR, CONST_LONG, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.eval
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 20, time = 1)
@Measurement(iterations = 20, time = 1)
class MerkleBenchmark {
  @Benchmark
  def createMerkleRoot(st: MerkleBenchmark.MerkleTreeData, bh: Blackhole): Unit =
    bh.consume(require(eval(st.expr).asInstanceOf[CONST_BYTESTR].bs == st.root))
}

object MerkleBenchmark {
  @State(Scope.Benchmark)
  class MerkleTreeData {
    @Param(Array("2", "4", "8", "16"))
    var depth = 0
    var expr: EXPR = uninitialized
    var root: ByteStr = uninitialized

    @Setup def setup(): Unit = {
      val leaves = (1 to BigInt(2).pow(depth).toInt).map(i => Ints.toByteArray(i))
      val levels = mkLevels(leaves)
      val proofs = mkProofs(leaves.size - 1, levels)
      val item = fastHash(Ints.toByteArray(leaves.size))
      root = ByteStr(levels.head.head)
      expr = FUNCTION_CALL(
        Native(FunctionIds.CREATE_MERKLE_ROOT),
        List(
          ARR(proofs.reverse.map(d => CONST_BYTESTR(ByteStr(d)).explicitGet()).toIndexedSeq, limited = true).explicitGet(),
          CONST_BYTESTR(ByteStr(item)).explicitGet(),
          CONST_LONG(leaves.size - 1)
        )
      )
    }
  }
}
