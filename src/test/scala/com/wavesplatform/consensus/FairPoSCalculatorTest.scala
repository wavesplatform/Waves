package com.wavesplatform.consensus

import org.scalatest.{Matchers, PropSpec}

import scala.util.Random

class FairPoSCalculatorTest extends PropSpec with Matchers {

  val pos = FairPoSCalculator

  case class Block(height: Int, hit: BigInt, baseTarget: Long, timestamp: Long)

  val signature = {
    println("called")
    Array.fill(32)(0: Byte)
  }

  def generationSignature: Array[Byte] = {
    val arr = Array[Byte](32)
    Random.nextBytes(arr)
    arr
  }

  property("...") {
    List.fill(10)(generationSignature) map
      (_.mkString("[", "", "]")) foreach
      println
  }

  property("") {

    val balance = 50000000000L

    val blockDelaySeconds = 60

    val defaultBaseTarget = 100L
    val height            = 0

    val first = calculateBlock(height, defaultBaseTarget, System.currentTimeMillis(), balance, blockDelaySeconds)

    val chain = (1 to 1000 foldLeft List(first)) {
      case (acc @ last :: _, _) =>
        val next =
          calculateBlock(
            last.height,
            last.baseTarget,
            last.timestamp,
            balance,
            blockDelaySeconds
          )

        next :: acc
    }

    chain.sortBy(-_.timestamp) shouldBe chain

    chain foreach println
  }

  def calculateBlock(prevHeight: Int, prevBaseTarget: Long, prevTimestamp: Long, balance: Long, blockDelaySeconds: Long): Block = {
    val hit  = pos.hit(generationSignature)
    val time = pos.time(hit, prevBaseTarget, balance)
    val bt   = pos.baseTarget(blockDelaySeconds, prevHeight + 1, prevBaseTarget, prevTimestamp, None, prevTimestamp)

    Block(
      prevHeight + 1,
      hit,
      bt,
      prevTimestamp + time
    )
  }
}
