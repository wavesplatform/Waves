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

  property("Correct consensus parameters of blocks generated with FairPoS") {

    val balance = 50000000L * 100000000L

    val blockDelaySeconds = 60

    val defaultBaseTarget = 100L

    val first = Block(0, defaultBaseTarget, System.currentTimeMillis(), 0)

    val chain = (1 to 10000 foldLeft List(first))((acc, _) => {
      acc match {
        case last :: _ =>
          val delay = pos.validBlockDelay(generationSignature, last.baseTarget, balance)
          val bt = pos.baseTarget(
            blockDelaySeconds,
            last.height + 1,
            last.baseTarget,
            last.timestamp,
            if (acc.isDefinedAt(2)) Some(acc(2).timestamp) else None,
            last.timestamp + delay
          )

          Block(
            last.height + 1,
            bt,
            last.timestamp + delay,
            delay
          ) :: acc

        case _ => ???
      }

    }).reverse

    val maxBT = chain.maxBy(_.baseTarget).baseTarget
    val avgBT = chain.map(_.baseTarget).sum / chain.length
    val minBT = chain.minBy(_.baseTarget).baseTarget

    val maxDelay = chain.tail.maxBy(_.delay).delay
    val avgDelay = chain.tail.map(_.delay).sum / (chain.length - 1)
    val minDelay = chain.tail.minBy(_.delay).delay

    print(
      s"""
        |BT: $minBT $avgBT $maxBT
        |Delay: $minDelay $avgDelay $maxDelay
      """.stripMargin
    )

    assert(avgDelay < 80000 && avgDelay > 40000)
    assert(avgBT < 200 && avgBT > 20)
  }

}
