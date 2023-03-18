package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.riderunner.input.EmptyPublicKey
import com.wavesplatform.riderunner.storage.Storage

class BlockPersistentCacheTestSuite extends PersistentTestSuite {
  "BlockPersistentCache" - {
    "get on empty return None" in test { (db, cache) =>
      db.readOnly { implicit ctx =>
        cache.get(1) shouldBe empty
      }
    }

    "set" - {
      "known height" - {
        "affects get" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(1, defaultHeader(2))
          }

          db.readWrite { implicit ctx =>
            cache.set(1, defaultHeader(20))
          }

          db.readOnly { implicit ctx =>
            cache.get(1).value.header.timestamp shouldBe 20
          }
        }

        "doesn't affect getLastHeight" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(1, defaultHeader(2))
          }

          db.readWrite { implicit ctx =>
            cache.set(1, defaultHeader(20))
          }

          db.readOnly { implicit ctx =>
            cache.getLastHeight.value shouldBe 1
          }
        }
      }

      "new height" - {
        "to empty" - {
          "affects get" in test { (db, cache) =>
            db.readWrite { implicit ctx =>
              cache.set(1, defaultHeader(2))
            }

            db.readOnly { implicit ctx =>
              cache.get(1).value.header.timestamp shouldBe 2
            }
          }

          "affects getLastHeight" in test { (db, cache) =>
            db.readWrite { implicit ctx =>
              cache.set(10, defaultHeader(2))
            }

            db.readOnly { implicit ctx =>
              cache.getLastHeight.value shouldBe 10
            }
          }
        }

        "to not empty" - {
          "affects get" in test { (db, cache) =>
            db.readWrite { implicit ctx =>
              cache.set(1, defaultHeader(2))
            }

            db.readWrite { implicit ctx =>
              cache.set(2, defaultHeader(4))
            }

            db.readOnly { implicit ctx =>
              cache.get(2).value.header.timestamp shouldBe 4
            }
          }

          "affects getLastHeight" in test { (db, cache) =>
            db.readWrite { implicit ctx =>
              cache.set(9, defaultHeader(2))
            }

            db.readWrite { implicit ctx =>
              cache.set(10, defaultHeader(11))
            }

            db.readOnly { implicit ctx =>
              cache.getLastHeight.value shouldBe 10
            }
          }
        }
      }
    }

    "remove" - {
      "known height" - {
        "affects get" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(1, defaultHeader(2))
          }

          db.readWrite { implicit ctx =>
            cache.set(2, defaultHeader(4))
          }

          db.readWrite { implicit ctx =>
            cache.removeFrom(2)
          }

          db.readOnly { implicit ctx =>
            cache.get(2) shouldBe empty
            cache.get(1).value.header.timestamp shouldBe 2
          }
        }

        "affects getLastHeight" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(1, defaultHeader(2))
          }

          db.readWrite { implicit ctx =>
            cache.set(2, defaultHeader(4))
          }

          db.readWrite { implicit ctx =>
            cache.removeFrom(2)
          }

          db.readOnly { implicit ctx =>
            cache.getLastHeight.value shouldBe 1
          }
        }
      }

      "new height" - {
        "doesn't affect get" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(1, defaultHeader(2))
          }

          db.readWrite { implicit ctx =>
            cache.removeFrom(2)
          }

          db.readOnly { implicit ctx =>
            cache.get(1).value.header.timestamp shouldBe 2
          }
        }

        "doesn't affect getLastHeight" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(1, defaultHeader(2))
          }

          db.readWrite { implicit ctx =>
            cache.removeFrom(2)
          }

          db.readOnly { implicit ctx =>
            cache.getLastHeight.value shouldBe 1
          }
        }
      }

      "clears" - {
        "affects get" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(1, defaultHeader(2))
          }

          db.readWrite { implicit ctx =>
            cache.removeFrom(1)
          }

          db.readOnly { implicit ctx =>
            cache.get(1) shouldBe empty
          }
        }

        "affects getLastHeight" in test { (db, cache) =>
          db.readWrite { implicit ctx =>
            cache.set(1, defaultHeader(2))
          }

          db.readWrite { implicit ctx =>
            cache.removeFrom(1)
          }

          db.readOnly { implicit ctx =>
            cache.getLastHeight shouldBe empty
          }
        }
      }
    }

    "getFrom" - {
      "returns Nil if empty" in test { (db, cache) =>
        db.readOnly { implicit ctx =>
          cache.getFrom(1, 100) shouldBe empty
        }
      }

      "returns headers if non empty" in test { (db, cache) =>
        db.readWrite { implicit ctx =>
          (1 to 25).foreach(i => cache.set(i, defaultHeader(i)))
        }

        val expected = (5L until 15).map(defaultHeader).toList
        db.readOnly { implicit ctx =>
          cache.getFrom(5, 10) shouldBe expected
        }
      }
    }
  }

  private def defaultHeader(ts: Long) =
    SignedBlockHeader(
      BlockHeader(0, ts, ByteStr.empty, 0, ByteStr.empty, EmptyPublicKey, Vector.empty, 0, ByteStr.empty),
      ByteStr.empty
    )

  private def test(f: (Storage, BlockPersistentCache) => Unit): Unit = withDb { db =>
    val caches = db.readOnly(DefaultPersistentCaches(db)(_))
    f(db, caches.blockHeaders)
  }
}
