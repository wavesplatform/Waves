package com.wavesplatform.storage.persistent

import com.wavesplatform.BaseTestSuite
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.input.EmptyPublicKey

class BlockPersistentCacheTestSuite extends BaseTestSuite with HasLevelDb {
  "BlockPersistentCache" - {
    "get on empty return None" in test { cache =>
      cache.get(1) shouldBe empty
    }

    "set" - {
      "known height" - {
        "affects get" in test { cache =>
          cache.set(1, defaultHeader(2))
          cache.set(1, defaultHeader(20))

          cache.get(1).value.header.timestamp shouldBe 20
        }

        "doesn't affect getLastHeight" in test { cache =>
          cache.set(1, defaultHeader(2))
          cache.set(1, defaultHeader(20))

          cache.getLastHeight.value shouldBe 1
        }
      }

      "new height" - {
        "to empty" - {
          "affects get" in test { cache =>
            cache.set(1, defaultHeader(2))
            cache.get(1).value.header.timestamp shouldBe 2
          }

          "affects getLastHeight" in test { cache =>
            cache.set(10, defaultHeader(2))
            cache.getLastHeight.value shouldBe 10
          }
        }

        "to not empty" - {
          "affects get" in test { cache =>
            cache.set(1, defaultHeader(2))
            cache.set(2, defaultHeader(4))
            cache.get(2).value.header.timestamp shouldBe 4
          }

          "affects getLastHeight" in test { cache =>
            cache.set(9, defaultHeader(2))
            cache.set(10, defaultHeader(11))
            cache.getLastHeight.value shouldBe 10
          }
        }
      }
    }

    "remove" - {
      "known height" - {
        "affects get" in test { cache =>
          cache.set(1, defaultHeader(2))
          cache.set(2, defaultHeader(4))
          cache.remove(2)

          cache.get(2) shouldBe empty
          cache.get(1).value.header.timestamp shouldBe 2
        }

        "affects getLastHeight" in test { cache =>
          cache.set(1, defaultHeader(2))
          cache.set(2, defaultHeader(4))
          cache.remove(2)

          cache.getLastHeight.value shouldBe 1
        }
      }

      "new height" - {
        "doesn't affect get" in test { cache =>
          cache.set(1, defaultHeader(2))
          cache.remove(2)

          cache.get(1).value.header.timestamp shouldBe 2
        }

        "doesn't affect getLastHeight" in test { cache =>
          cache.set(1, defaultHeader(2))
          cache.remove(2)

          cache.getLastHeight.value shouldBe 1
        }
      }

      "clears" - {
        "affects get" in test { cache =>
          cache.set(1, defaultHeader(2))
          cache.remove(1)

          cache.get(1) shouldBe empty
        }

        "affects getLastHeight" in test { cache =>
          cache.set(1, defaultHeader(2))
          cache.remove(1)

          cache.getLastHeight shouldBe empty
        }
      }
    }
  }

  private def defaultHeader(ts: Long = 0) =
    SignedBlockHeader(
      BlockHeader(0, ts, ByteStr.empty, 0, ByteStr.empty, EmptyPublicKey, Nil, 0, ByteStr.empty),
      ByteStr.empty
    )

  private def test(f: BlockPersistentCache => Unit): Unit = withDb { db =>
    val caches = new LevelDbPersistentCaches(db)
    f(caches.blockHeaders)
  }
}
