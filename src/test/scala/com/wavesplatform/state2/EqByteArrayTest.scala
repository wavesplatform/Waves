package com.wavesplatform.state2

import com.wavesplatform.state2.StateStorage.SnapshotKey
import org.h2.mvstore.{MVMap, MVStore}
import org.scalatest.{FunSuite, Matchers}

class EqByteArrayTest extends FunSuite with Matchers {
  test("put/get value by key") {
    val map = new MVStore.Builder().open().openMap("mapName", new MVMap.Builder[EqByteArray, Int].keyType(new EqByteArrayMVStoreDataType))

    val key = EqByteArray(Array(1: Byte))
    val sameKey = EqByteArray(Array(1: Byte))
    val otherKey = EqByteArray(Array(1: Byte, 1: Byte))

    map.put(key, 1)
    map.get(key) shouldBe 1
    map.get(sameKey) shouldBe 1
    Option(map.get(otherKey)) shouldBe None
    map.put(otherKey, 2)
    map.get(otherKey) shouldBe 2
  }

}
