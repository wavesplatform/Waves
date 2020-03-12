package com.wavesplatform.transaction.merkle

import java.util.concurrent.TimeUnit

import com.wavesplatform.block.merkle.Merkle
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

//noinspection ScalaStyle
@State(Scope.Thread)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class MerkleBenchmark {

  val messages: Seq[Array[Byte]] = (0 until 2048).map { _ =>
    val message = new Array[Byte](2048)
    Random.nextBytes(message)
    message
  }

  val levels: Seq[Seq[Array[Byte]]] = Merkle.mkLevels(messages)

  val messageIdx: Int            = Random.nextInt(2048)
  val messageDigest: Array[Byte] = Merkle.hash(messages(messageIdx))
  val proof: Seq[Array[Byte]]    = Merkle.mkProofs(messageIdx, levels)

  @Benchmark
  def merkleMkLevels_test(bh: Blackhole): Unit = {
    val tree = Merkle.mkLevels(messages)
    bh.consume(tree)
  }

  @Benchmark
  def merkleMkProof_test(bh: Blackhole): Unit = {
    val proof = Merkle.mkProofs(Random.nextInt(2048), levels)
    bh.consume(proof)
  }

  @Benchmark
  def merkleVerifyProof_test(bh: Blackhole): Unit = {
    val result = Merkle.verify(messageDigest, messageIdx, proof, levels.head.head)
    bh.consume(result)
  }
}
