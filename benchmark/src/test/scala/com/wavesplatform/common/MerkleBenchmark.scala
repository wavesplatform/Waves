package com.wavesplatform.common

import java.util.concurrent.TimeUnit

import com.google.common.primitives.Ints
import com.wavesplatform.common.merkle.Merkle._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.hash.{Blake2b256, CryptographicHash32, Digest32}

import scala.util.Random

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class MerkleBenchmark {

  @Benchmark
  def merkelCreateRoot4(st: MerkleBenchmark.Merkle4St, bh: Blackhole): Unit =
    bh.consume(st.proofs.map({ case (proof, value, index) => createRoot(value, index, proof) }))

  @Benchmark
  def merkelCreateRoot6(st: MerkleBenchmark.Merkle6St, bh: Blackhole): Unit =
    bh.consume(st.proofs.map({ case (proof, value, index) => createRoot(value, index, proof) }))

  @Benchmark
  def merkelCreateRoot8(st: MerkleBenchmark.Merkle8St, bh: Blackhole): Unit =
    bh.consume(st.proofs.map({ case (proof, value, index) => createRoot(value, index, proof) }))

  @Benchmark
  def merkelCreateRoot10(st: MerkleBenchmark.Merkle10St, bh: Blackhole): Unit =
    bh.consume(st.proofs.map({ case (proof, value, index) => createRoot(value, index, proof) }))

  @Benchmark
  def merkelCreateRoot12(st: MerkleBenchmark.Merkle12St, bh: Blackhole): Unit =
    bh.consume(st.proofs.map({ case (proof, value, index) => createRoot(value, index, proof) }))

  @Benchmark
  def merkelCreateRoot14(st: MerkleBenchmark.Merkle14St, bh: Blackhole): Unit =
    bh.consume(st.proofs.map({ case (proof, value, index) => createRoot(value, index, proof) }))

  @Benchmark
  def merkelCreateRoot16(st: MerkleBenchmark.Merkle16St, bh: Blackhole): Unit =
    bh.consume(st.proofs.map({ case (proof, value, index) => createRoot(value, index, proof) }))
}

object MerkleBenchmark {
  implicit val fastHash = new CryptographicHash32 {
    override def hash(input: Message): Digest32 = Blake2b256.hash(input)
  }

  def testData(deep: Int): (MerkleTree[Digest32], List[LeafData]) = {
    val n    = BigInt(2).pow(deep - 1).toInt
    val size = n + 1 + Random.nextInt(n)
    val data: List[LeafData] =
      List
        .fill(size)(Random.nextInt(10000))
        .map(Ints.toByteArray)
        .map(LeafData @@ _)

    val tree = MerkleTree[Digest32](data)(fastHash)

    (tree, data)
  }

  class MerkleDeep(size: Int) {
    val leafs  = testData(size)._2
    val levels = mkLevels(leafs)
    val root   = levels.head.head
    val proofs = List(
      (mkProofs(0, levels), hash(leafs(0)), 0),
      (mkProofs(size - 1, levels), hash(leafs(size - 1)), size - 1),
      (mkProofs(size / 2, levels), hash(leafs(size / 2)), size / 2)
    )
  }

  @State(Scope.Benchmark)
  class Merkle4St extends MerkleDeep(4)

  @State(Scope.Benchmark)
  class Merkle6St extends MerkleDeep(6)

  @State(Scope.Benchmark)
  class Merkle8St extends MerkleDeep(8)

  @State(Scope.Benchmark)
  class Merkle10St extends MerkleDeep(10)

  @State(Scope.Benchmark)
  class Merkle12St extends MerkleDeep(12)

  @State(Scope.Benchmark)
  class Merkle14St extends MerkleDeep(14)

  @State(Scope.Benchmark)
  class Merkle16St extends MerkleDeep(16)
}
