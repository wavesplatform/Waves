package com.wavesplatform.ride.runner.db

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.ride.runner.storage.persistent.HasDb

class ReadOnlyTestSuite extends BaseTestSuite with HasDb {
  "ReadOnly" - {
    "prefixExists" in withDb { db =>
      val prefix   = Array[Byte](1)
      val rawKey   = prefix ++ Array[Byte](2)
      val rawValue = Array[Byte](3)

      def check(clue: String, exist: Boolean): Unit = db.readOnly { ro =>
        withClue(s"$clue:") {
          ro.prefixExists(rawKey) shouldBe exist
          ro.prefixExists(prefix) shouldBe exist
        }
      }

      check("before insert", exist = false)

      db.readWrite { rw =>
        rw.put(rawKey, rawValue)
      }

      check("after insert", exist = true)

      db.readWrite { rw =>
        rw.delete(rawKey)
      }

      check("after delete", exist = false)
    }
  }
}
