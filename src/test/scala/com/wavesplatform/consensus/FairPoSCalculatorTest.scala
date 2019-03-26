package com.wavesplatform.consensus

import cats.data.NonEmptyList
import cats.implicits._
import com.wavesplatform.account.KeyPair
import org.scalatest.{Matchers, PropSpec}

import scala.util.Random

class FairPoSCalculatorTest extends PropSpec with Matchers {

  import PoSCalculator._

  val pos: PoSCalculator = FairPoSCalculator

  case class Block(height: Int, baseTarget: Long, miner: KeyPair, timestamp: Long, delay: Long)

  def generationSignature: Array[Byte] = {
    val arr = new Array[Byte](32)
    Random.nextBytes(arr)
    arr
  }

  val balance: Long     = 50000000L * 100000000L
  val blockDelaySeconds = 60
  val defaultBaseTarget = 100L

  property("Correct consensus parameters distribution of blocks generated with FairPoS") {

    val miners = mkMiners
    val first  = Block(0, defaultBaseTarget, KeyPair(generationSignature), System.currentTimeMillis(), 0)

    val chain = (1 to 100000 foldLeft NonEmptyList.of(first))((acc, _) => {
      val gg     = acc.tail.lift(1)
      val blocks = miners.map(mineBlock(acc.head, gg, _))

      val next = blocks.minBy(_.delay)

      next :: acc
    }).reverse.tail

    val avgBT    = chain.map(_.baseTarget).sum / chain.length
    val avgDelay = chain.tail.map(_.delay).sum / (chain.length - 1)

    val minersPerformance = calcPerfomance(chain, miners)

    assert(minersPerformance.forall(p => p._2 < 1.1 && p._2 > 0.9))
    assert(avgDelay < 80000 && avgDelay > 40000)
    assert(avgBT < 200 && avgBT > 20)
  }

  def mineBlock(prev: Block, grand: Option[Block], minerWithBalance: (KeyPair, Long)): Block = {
    val (miner, balance) = minerWithBalance
    val gs               = generatorSignature(generationSignature, miner)
    val h                = hit(gs)
    val delay            = pos.calculateDelay(h, prev.baseTarget, balance)
    val bt = pos.calculateBaseTarget(
      blockDelaySeconds,
      prev.height + 1,
      prev.baseTarget,
      prev.timestamp,
      grand.map(_.timestamp),
      prev.timestamp + delay
    )

    Block(
      prev.height + 1,
      bt,
      miner,
      prev.timestamp + delay,
      delay
    )
  }

  def calcPerfomance(chain: List[Block], miners: Map[KeyPair, Long]): Map[Long, Double] = {
    val balanceSum  = miners.values.sum
    val blocksCount = chain.length

    chain
      .groupBy(_.miner)
      .map(mbs => {
        val (miner, blocks) = mbs

        val minerBalance   = miners(miner)
        val expectedBlocks = ((minerBalance.toDouble / balanceSum) * blocksCount).toLong
        val perfomance     = blocks.length.toDouble / expectedBlocks

        minerBalance -> perfomance
      })
  }

  def mkMiners: Map[KeyPair, Long] =
    List(
      KeyPair(generationSignature) -> 200000000000000L,
      KeyPair(generationSignature) -> 500000000000000L,
      KeyPair(generationSignature) -> 1000000000000000L,
      KeyPair(generationSignature) -> 1500000000000000L,
      KeyPair(generationSignature) -> 2000000000000000L,
      KeyPair(generationSignature) -> 2500000000000000L
    ).toMap
}
