package com.wavesplatform.database

import com.wavesplatform.db.WithState
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.utils.*

class RWSpec extends FreeSpec with WithState {
  private def bytes(s: String): Array[Byte] = s.utf8Bytes

  private def valueOf(rdb: RDB, key: String): Option[String] = Option(rdb.db.get(bytes(key))).map(new String(_, "UTF-8"))

  "a batch leaves the same state as its operations applied in order" - {
    "the last write to a key wins" in tempDb { rdb =>
      rdb.db.readWrite { rw =>
        rw.put(bytes("b"), bytes("first"))
        rw.put(bytes("a"), bytes("other"))
        rw.delete(bytes("b"))
        rw.put(bytes("b"), bytes("last"))
        rw.put(bytes("c"), bytes("deleted"))
        rw.delete(bytes("c"))
      }

      (valueOf(rdb, "a"), valueOf(rdb, "b"), valueOf(rdb, "c")) shouldBe (Some("other"), Some("last"), None)
    }

    "a write after a range delete over its key survives" in tempDb { rdb =>
      rdb.db.readWrite { rw =>
        rw.put(bytes("k1"), bytes("before"))
        rw.deleteRange(bytes("k0"), bytes("k9"))
        rw.put(bytes("k2"), bytes("after"))
      }

      (valueOf(rdb, "k1"), valueOf(rdb, "k2")) shouldBe (None, Some("after"))
    }
  }
}
