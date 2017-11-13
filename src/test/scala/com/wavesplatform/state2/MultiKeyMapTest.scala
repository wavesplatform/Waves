package com.wavesplatform.state2

import com.wavesplatform.TransactionGen
import org.h2.mvstore.MVStore
import org.h2.mvstore.`type`.ObjectDataType
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class MultiKeyMapTest extends PropSpec with PropertyChecks with TransactionGen with Matchers {
  def newMap() = new MultiKeyMap[Long](new MVStore.Builder().open(), new ObjectDataType(), "p")

  val addressByteStr: Gen[ByteStr] = accountGen.map(_.toAddress.bytes)
  val assetByteStr: Gen[ByteStr] = byteArrayGen(64).map(ByteStr(_))

  property("can store value") {
    forAll(addressByteStr, assetByteStr, positiveLongGen) { (acc, asset, amt) =>
      val m = newMap()
      m.put(acc, asset, amt)
      m.get(acc, asset) shouldBe Some(amt)
    }
  }

  property("can overwrite value") {
    forAll(addressByteStr, assetByteStr, positiveLongGen, positiveLongGen) { (acc, asset, amt, amt2) =>
      val m = newMap()
      m.put(acc, asset, amt)
      m.put(acc, asset, amt2)
      m.get(acc, asset) shouldBe Some(amt2)
    }
  }

  property("can add second key") {
    forAll(addressByteStr, assetByteStr, assetByteStr, positiveLongGen, positiveLongGen) { (acc, asset, asset2, amt, amt2) =>
      val m = newMap()
      m.put(acc, asset, amt)
      m.put(acc, asset2, amt2)
      m.get(acc, asset) shouldBe Some(amt)
      m.get(acc, asset2) shouldBe Some(amt2)
    }
  }

  property("can add, modify multiple keys, can retrieve values") {
    forAll(addressByteStr, addressByteStr, assetByteStr, assetByteStr, positiveLongGen, positiveLongGen) { (acc, acc2, asset, asset2, amt, amt2) =>
      val m = newMap()
      m.put(acc, asset, amt)
      m.put(acc, asset2, amt2)
      m.put(acc2, asset, amt)

      m.keys(acc) shouldBe Set(asset, asset2)
      m.keys(acc2) shouldBe Set(asset)


      m.getMap(acc) shouldBe Map(asset -> amt, asset2 -> amt2)
      m.getMap(acc2) shouldBe Map(asset -> amt)

    }
  }

}
