package com.wavesplatform.state2

import com.wavesplatform.TransactionGen
import org.h2.mvstore.WriteBuffer
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class DataTypesSpec extends PropSpec with PropertyChecks with TransactionGen with Matchers {

  property("mapShortInt round trip") {

    val shortToIntGen = for {
      s <- Arbitrary.arbitrary[Short]
      i <- Arbitrary.arbitrary[Int]
    } yield {s -> i}

    val mapGen = for {
      size <- Gen.choose(1, 100)
      list <- Gen.listOfN(size, shortToIntGen)
    } yield {list.toMap}

    forAll(mapGen) { map =>
      val wb = new WriteBuffer()

      DataTypes.mapShortInt.write(wb, map)

      wb.position(0)

      DataTypes.mapShortInt.read(wb.getBuffer) shouldEqual map
    }
  }

}
