package com.wavesplatform.ride.runner.db

import com.google.common.primitives.Shorts
import com.wavesplatform.BaseTestSuite
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.runner.caches.persistent.HasDb

abstract class ReadOnlyTestSuite extends BaseTestSuite with HasDb {
  private val testName      = getClass.getSimpleName.replace("TestSuite", "")
  private val defaultPrefix = Shorts.toByteArray(1)
  private val otherPrefix   = Shorts.toByteArray(2)
  private val defaultValue  = Array[Byte](3)

  testName - {
    "iterateOverPrefix" in withDb { db =>
      val k1                 = mkKey(1)
      val k2                 = mkKey(2)
      val k3                 = mkKey(3)
      val keyWithOtherPrefix = mkKey(4, otherPrefix)

      def check(clue: String, prefix: Array[Byte] = defaultPrefix, expectedKeys: List[ByteStr]): Unit = readOnly(db) { ro =>
        withClue(s"$clue:") {
          var keys = List.empty[ByteStr]
          ro.iterateOverPrefix(prefix, None) { x =>
            keys = ByteStr(x.getKey) :: keys
          }

          keys should contain theSameElementsAs expectedKeys
        }
      }

      check("before insert", expectedKeys = List.empty)

      readWrite(db) { rw =>
        rw.put(keyWithOtherPrefix.arr, defaultValue, None)
      }

      check("after insert with other prefix", expectedKeys = List.empty)

      readWrite(db) { rw =>
        rw.put(k1.arr, defaultValue, None)
        rw.put(k2.arr, defaultValue, None)
        rw.put(k3.arr, defaultValue, None)
      }

      check("after insert with 2-byte prefix", expectedKeys = List(k1, k2, k3))
      check("after insert with a key", k2.arr, expectedKeys = List(k2, k3))

      readWrite(db) { rw =>
        rw.delete(k2.arr, None)
      }

      check("after delete", expectedKeys = List(k1, k3))
    }

    "prefixExists" in withDb { db =>
      val rawKey = mkKey(2)

      def check(clue: String, exist: Boolean): Unit = readOnly(db) { ro =>
        withClue(s"$clue:") {
          ro.prefixExists(rawKey.arr, None) shouldBe exist
          ro.prefixExists(defaultPrefix, None) shouldBe exist
        }
      }

      check("before insert", exist = false)

      readWrite(db) { rw =>
        rw.put(mkKey(3, otherPrefix).arr, defaultValue, None)
      }

      check("after insert with other prefix", exist = false)

      readWrite(db) { rw =>
        rw.put(rawKey.arr, defaultValue, None)
      }

      check("after insert with expected prefix", exist = true)

      readWrite(db) { rw =>
        rw.delete(rawKey.arr, None)
      }

      check("after delete", exist = false)
    }
  }

  private def mkKey(x: Byte, prefix: Array[Byte] = defaultPrefix) = ByteStr(prefix ++ Array[Byte](x))

  protected def readOnly[T](dbAccess: RideDbAccess)(f: ReadOnly => T): T
  protected def readWrite[T](dbAccess: RideDbAccess)(f: ReadWrite => T): T
}
