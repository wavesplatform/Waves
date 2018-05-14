package com.wavesplatform.consensus

import org.scalatest.{Matchers, PropSpec}

import scala.util.Random

class FairPoSCalculatorTest extends PropSpec with Matchers {

  val pos = FairPoSCalculator

  case class Block(height: Int, baseTarget: Long, timestamp: Long, delay: Long)

  def generationSignature: Array[Byte] = {
    val arr = new Array[Byte](32)
    Random.nextBytes(arr)
    arr
  }

  property("") {

    val balance = 50000000000L * 100000000L

    val blockDelaySeconds = 60

    val defaultBaseTarget = 100L

    val first = Block(0, defaultBaseTarget, System.currentTimeMillis(), 0)

    val chain = (1 to 1000 foldLeft List(first)) {
      case (acc @ last :: _, h) =>
        val next =
          calculateBlock(
            last.height,
            last.baseTarget,
            last.timestamp,
            if (h > 3) Some(acc(3).timestamp) else None,
            balance,
            blockDelaySeconds
          )

        next :: acc
    }

    chain.sortBy(-_.timestamp) shouldBe chain

    chain foreach println
  }

  def calculateBlock(prevHeight: Int, prevBaseTarget: Long, prevTimestamp: Long,
                     greatParentTimestamp: Option[Long], balance: Long, targetBlockDelaySeconds: Long): Block = {
    val hit  = pos.hit(generationSignature)
    val delay = pos.time(hit, prevBaseTarget, balance)
    val bt   = pos.baseTarget(targetBlockDelaySeconds, prevHeight + 1, prevBaseTarget, prevTimestamp, greatParentTimestamp, prevTimestamp + delay)

    Block(
      prevHeight + 1,
      bt,
      prevTimestamp + delay,
      delay
    )
  }


}
