package com.wavesplatform.transaction.merkle

import java.util.concurrent.TimeUnit

import com.wavesplatform.common.merkle.Merkle
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

  val messageSize = 2048
  val evenSize    = 2048
  val oddSize     = 2047

  val messagesEven: Seq[Array[Byte]] = (0 until evenSize).map { _ =>
    val message = new Array[Byte](messageSize)
    Random.nextBytes(message)
    message
  }

  val messagesOdd: Seq[Array[Byte]] = (0 until oddSize).map { _ =>
    val message = new Array[Byte](messageSize)
    Random.nextBytes(message)
    message
  }

  val levelsEven: Seq[Seq[Array[Byte]]] = Merkle.mkLevels(messagesEven)
  val levelsOdd: Seq[Seq[Array[Byte]]]  = Merkle.mkLevels(messagesOdd)

  val messageIdxEven: Int            = Random.nextInt(evenSize)
  val messageIdxOdd: Int             = Random.nextInt(oddSize)
  val messageDigestEven: Array[Byte] = Merkle.hash(messagesEven(messageIdxEven))
  val messageDigestOdd: Array[Byte]  = Merkle.hash(messagesOdd(messageIdxOdd))
  val proofEven: Seq[Array[Byte]]    = Merkle.mkProofs(messageIdxEven, levelsEven)
  val proofOdd: Seq[Array[Byte]]     = Merkle.mkProofs(messageIdxOdd, levelsOdd)

  @Benchmark
  def merkleMkLevelsEven_test(bh: Blackhole): Unit = {
    val tree = Merkle.mkLevels(messagesEven)
    bh.consume(tree)
  }

  @Benchmark
  def merkleMkProofEven_test(bh: Blackhole): Unit = {
    val proof = Merkle.mkProofs(Random.nextInt(evenSize), levelsEven)
    bh.consume(proof)
  }

  @Benchmark
  def merkleVerifyProofEven_test(bh: Blackhole): Unit = {
    val result = Merkle.verify(messageDigestEven, messageIdxEven, proofEven, levelsEven.head.head)
    bh.consume(result)
  }

  @Benchmark
  def merkleMkLevelsOdd_test(bh: Blackhole): Unit = {
    val tree = Merkle.mkLevels(messagesOdd)
    bh.consume(tree)
  }

  @Benchmark
  def merkleMkProofOdd_test(bh: Blackhole): Unit = {
    val proof = Merkle.mkProofs(Random.nextInt(oddSize), levelsOdd)
    bh.consume(proof)
  }

  @Benchmark
  def merkleVerifyProofOdd_test(bh: Blackhole): Unit = {
    val result = Merkle.verify(messageDigestOdd, messageIdxOdd, proofOdd, levelsOdd.head.head)
    bh.consume(result)
  }
}
