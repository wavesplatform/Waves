package com.wavesplatform.ride.runner.db

import com.google.common.primitives.Shorts
import com.wavesplatform.BaseTestSuite
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.runner.storage.persistent.HasDb

class ReadOnlyTestSuite extends BaseTestSuite with HasDb {
  private val defaultPrefix = Shorts.toByteArray(1)
  private val defaultValue  = Array[Byte](3)

  "ReadOnly" - {
    "iterateOverPrefix" in withDb { db =>
      val k1 = mkKey(1)
      val k2 = mkKey(2)
      val k3 = mkKey(3)

      def check(clue: String, prefix: Array[Byte] = defaultPrefix, expectedKeys: List[ByteStr]): Unit = db.readOnly { ro =>
        withClue(s"$clue:") {
          var keys = List.empty[ByteStr]
          ro.iterateOverPrefix(prefix) { x =>
            keys = ByteStr(x.getKey) :: keys
          }

          keys should contain theSameElementsAs expectedKeys
        }
      }

      check("before insert", expectedKeys = List.empty)

      db.readWrite { rw =>
        rw.put(k1.arr, defaultValue)
        rw.put(k2.arr, defaultValue)
        rw.put(k3.arr, defaultValue)
      }

      check("after insert with 2-byte prefix", expectedKeys = List(k1, k2, k3))
      check("after insert with a key", k2.arr, expectedKeys = List(k2, k3))

      db.readWrite { rw =>
        rw.delete(k2.arr)
      }

      check("after delete", expectedKeys = List(k1, k3))
    }

    "prefixExists" in withDb { db =>
      val rawKey = mkKey(2)

      def check(clue: String, exist: Boolean): Unit = db.readOnly { ro =>
        withClue(s"$clue:") {
          ro.prefixExists(rawKey.arr) shouldBe exist
          ro.prefixExists(defaultPrefix) shouldBe exist
        }
      }

      check("before insert", exist = false)

      db.readWrite { rw =>
        rw.put(rawKey.arr, defaultValue)
      }

      check("after insert", exist = true)

      db.readWrite { rw =>
        rw.delete(rawKey.arr)
      }

      check("after delete", exist = false)
    }
  }

  private def mkKey(x: Byte, prefix: Array[Byte] = defaultPrefix) = ByteStr(prefix ++ Array[Byte](x))
}
