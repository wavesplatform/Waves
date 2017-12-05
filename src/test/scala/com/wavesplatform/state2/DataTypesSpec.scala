package com.wavesplatform.state2

import org.h2.mvstore.WriteBuffer
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class DataTypesSpec extends PropSpec with PropertyChecks with Matchers {

  def mapGen[T1, T2](itemGen: Gen[(T1, T2)]): Gen[Map[T1, T2]] = for {
    size <- Gen.choose(1, 100)
    list <- Gen.listOfN(size, itemGen)
  } yield {
    list.toMap
  }

  property("mapShortInt round trip") {
    val shortToIntGen = for {
      s <- Arbitrary.arbitrary[Short]
      i <- Arbitrary.arbitrary[Int]
    } yield {
      s -> i
    }

    forAll(mapGen(shortToIntGen)) { map =>
      val wb = new WriteBuffer()

      DataTypes.mapShortInt.write(wb, map)

      wb.position(0)

      DataTypes.mapShortInt.read(wb.getBuffer) shouldEqual map
    }
  }
}
