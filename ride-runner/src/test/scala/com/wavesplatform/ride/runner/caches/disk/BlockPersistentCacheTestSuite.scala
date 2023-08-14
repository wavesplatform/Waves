package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.runner.db.RideDbAccess
import com.wavesplatform.state.Height

class BlockPersistentCacheTestSuite extends PersistentTestSuite {
  "BlockPersistentCache" - {
    "get on empty return None" in test { (db, cache) =>
      db.batchedReadOnly { implicit ctx =>
        cache.get(Height(1)) shouldBe empty
      }
    }

    "setLastHeight" in test { (db, cache) =>
      db.batchedReadWrite { implicit ctx =>
        cache.getLastHeight shouldBe empty
      }

      db.batchedReadWrite { implicit ctx =>
        cache.setLastHeight(Height(2))
      }

      db.batchedReadOnly { implicit ctx =>
        cache.getLastHeight.value shouldBe 2
      }
    }

    "set" - {
      "known height" - {
        "affects get" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(1), defaultHeader(2))
          }

          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(1), defaultHeader(20))
          }

          db.batchedReadOnly { implicit ctx =>
            cache.get(Height(1)).value.header.header.timestamp shouldBe 20
          }
        }
      }

      "new height" - {
        "to empty" - {
          "affects get" in test { (db, cache) =>
            db.batchedReadWrite { implicit ctx =>
              cache.set(Height(1), defaultHeader(2))
            }

            db.batchedReadOnly { implicit ctx =>
              cache.get(Height(1)).value.header.header.timestamp shouldBe 2
            }
          }
        }

        "to not empty" - {
          "affects get" in test { (db, cache) =>
            db.batchedReadWrite { implicit ctx =>
              cache.set(Height(1), defaultHeader(2))
            }

            db.batchedReadWrite { implicit ctx =>
              cache.set(Height(2), defaultHeader(4))
            }

            db.batchedReadOnly { implicit ctx =>
              cache.get(Height(2)).value.header.header.timestamp shouldBe 4
            }
          }
        }
      }
    }

    "remove" - {
      "known height" - {
        "affects get" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(1), defaultHeader(2))
          }

          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(2), defaultHeader(4))
          }

          db.batchedReadWrite { implicit ctx =>
            cache.removeFrom(Height(2))
          }

          db.batchedReadOnly { implicit ctx =>
            cache.get(Height(2)) shouldBe empty
            cache.get(Height(1)).value.header.header.timestamp shouldBe 2
          }
        }
      }

      "new height" - {
        "doesn't affect get" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(1), defaultHeader(2))
          }

          db.batchedReadWrite { implicit ctx =>
            cache.removeFrom(Height(2))
          }

          db.batchedReadOnly { implicit ctx =>
            cache.get(Height(1)).value.header.header.timestamp shouldBe 2
          }
        }
      }

      "clears" - {
        "affects get" in test { (db, cache) =>
          db.batchedReadWrite { implicit ctx =>
            cache.set(Height(1), defaultHeader(2))
          }

          db.batchedReadWrite { implicit ctx =>
            cache.removeFrom(Height(1))
          }

          db.batchedReadOnly { implicit ctx =>
            cache.get(Height(1)) shouldBe empty
          }
        }
      }
    }

    "getFrom" - {
      "returns Nil if empty" in test { (db, cache) =>
        db.batchedReadOnly { implicit ctx =>
          cache.getFrom(Height(1), 100) shouldBe empty
        }
      }

      "returns headers if non empty" in test { (db, cache) =>
        db.batchedReadWrite { implicit ctx =>
          (1 to 25).foreach(i => cache.set(Height(i), defaultHeader(i)))
        }

        val expected = (5L until 15).map(defaultHeader).toList
        db.batchedReadOnly { implicit ctx =>
          cache.getFrom(Height(5), 10) shouldBe expected
        }
      }
    }
  }

  private def defaultHeader(ts: Long) = SignedBlockHeaderWithVrf(
    SignedBlockHeader(
      BlockHeader(0, ts, ByteStr.empty, 0, ByteStr.empty, EmptyPublicKey, Vector.empty, 0, ByteStr.empty),
      ByteStr.empty
    ),
    vrf = ByteStr.empty,
    blockReward = 600_000_000L
  )

  private def test(f: (RideDbAccess, BlockPersistentCache) => Unit): Unit = withDb { db =>
    val caches = db.batchedReadOnly(DefaultPersistentCaches(db)(_))
    f(db, caches.blockHeaders)
  }
}
